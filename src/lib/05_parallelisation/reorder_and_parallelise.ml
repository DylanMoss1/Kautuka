open! Core
open Ast.Ast_types
open Util
open Side_effect_system
open Side_effect_system.Side_effect_tracking
open Preperation.Import
open Cost_analysis.Type_cost
open Ast.Annotated_ast
open Cost_analysis
open Cost_analysis.Cost

module Block_parallelised_annotation = struct
  type t =
    { side_effects : Side_effect_tracking.Side_effect_set.t
    ; scoped_vars : Alpha.t list
    ; runtime_cost : Cost.t
    ; parallelise_contents : int option
    }

  let string_of_parallelise_contents = function
    | Some i -> Fmt.str "yes (%d blocks)" i
    | None -> "no"


  let string_of_t t =
    Fmt.str
      "{side_effects: %s, scoped_vars: %s, runtime_cost: %s, \
       parallelise_contents: %s}"
      (Side_effect_set.string_of_t t.side_effects)
      (Alpha_conversion.string_of_scoped_vars t.scoped_vars)
      (Cost.string_of_t t.runtime_cost)
      (string_of_parallelise_contents t.parallelise_contents)
end

module Parallelisation_ast =
  Annotated_ast
    (Block_parallelised_annotation)
    (Alpha_conversion.Alpha_conversion_annotation)
    (Import_annotation)
    (Expr_type_cost_annotation)

let contains_parallelisation = ref false

let find_disjoint_blocks_collection
    (block_list : (Parallelisation_ast.block_annot, 'b, 'c) block list)
  =
  let rec find_disjoint_blocks_collection_inner
      (block_list : (Parallelisation_ast.block_annot, 'b, 'c) block list)
      collected_effects
      collected_blocks
      passed_blocks
    =
    match block_list with
    | block :: block_list ->
      let new_collected_effects =
        Side_effect_set.union block.annotations.side_effects collected_effects
      in
      let collected_blocks, passed_blocks =
        if Side_effect_set.disjoint
             block.annotations.side_effects
             collected_effects
        then collected_blocks @ [ block ], passed_blocks
        else collected_blocks, passed_blocks @ [ block ]
      in
      find_disjoint_blocks_collection_inner
        block_list
        new_collected_effects
        collected_blocks
        passed_blocks
    | [] -> collected_blocks, passed_blocks
  in
  find_disjoint_blocks_collection_inner block_list Side_effect_set.empty [] []


let find_all_disjoint_block_collections
    (block_list : (Parallelisation_ast.block_annot, 'b, 'c) block list)
  =
  let rec find_all_disjoint_block_collections_inner
      remaining_blocks
      all_disjoint_block_collections
    =
    if List.length remaining_blocks = 0
    then all_disjoint_block_collections
    else (
      let disjoint_block_collection, remaining_blocks =
        find_disjoint_blocks_collection remaining_blocks
      in
      find_all_disjoint_block_collections_inner
        remaining_blocks
        (List.append
           all_disjoint_block_collections
           [ disjoint_block_collection ]))
  in
  find_all_disjoint_block_collections_inner block_list []


exception Empty_disjoint_blocks_list

let wrap_in_block_structure block = Structure (Block_struct block)

let get_side_effects_of_contents
    (contents : (Block_parallelised_annotation.t, 'b, 'c) block list)
  =
  List.fold_left
    ~init:Side_effect_set.empty
    ~f:(fun acc block ->
      Side_effect_set.union acc block.annotations.side_effects)
    contents


let get_scoped_vars_of_contents
    (contents : (Block_parallelised_annotation.t, 'b, 'c) block list)
  =
  match contents with
  | block :: _ -> block.annotations.scoped_vars
  | [] -> raise Empty_disjoint_blocks_list


let get_runtime_cost_of_contents
    (contents : (Block_parallelised_annotation.t, 'b, 'c) block list)
  =
  List.fold_left
    ~init:Cost.zero
    ~f:(fun acc block -> Cost.sum acc block.annotations.runtime_cost)
    contents


let parallelise_disjoint_blocks
    (block_list : (Parallelisation_ast.block_annot, 'b, 'c) block list)
  =
  let disjoint_blocks_list = find_all_disjoint_block_collections block_list in
  List.map
    ~f:(fun disjoint_blocks ->
      match disjoint_blocks with
      | [] -> raise Empty_disjoint_blocks_list
      | [ block ] -> wrap_in_block_structure block
      | disjoint_blocks ->
        contains_parallelisation := true;
        wrap_in_block_structure
          { contents = List.map ~f:wrap_in_block_structure disjoint_blocks
          ; annotations =
              { side_effects = get_side_effects_of_contents disjoint_blocks
              ; scoped_vars = get_scoped_vars_of_contents disjoint_blocks
              ; runtime_cost = get_runtime_cost_of_contents disjoint_blocks
              ; parallelise_contents = Some (List.length disjoint_blocks)
              }
          })
    disjoint_blocks_list


let rec parallelise_for_loop for_loop =
  { init = for_loop.init
  ; cond = for_loop.cond
  ; iter = for_loop.iter
  ; contents = parallelise_block for_loop.contents
  }


and parallelise_for_each for_each =
  { item = for_each.item
  ; iterator = for_each.iterator
  ; contents = parallelise_block for_each.contents
  }


and parallelise_condition_template condition_template =
  { condition = condition_template.condition
  ; contents = parallelise_block condition_template.contents
  }


and parallelise_if_record if_record =
  { _if = parallelise_condition_template if_record._if
  ; else_if = List.map ~f:parallelise_condition_template if_record.else_if
  ; else_contents = Option.map ~f:parallelise_block if_record.else_contents
  }


and parallelise_structure = function
  | Block_struct block -> Block_struct (parallelise_block block)
  | If if_record -> If (parallelise_if_record if_record)
  | For_loop for_loop -> For_loop (parallelise_for_loop for_loop)
  | For_each for_each -> For_each (parallelise_for_each for_each)


and parallelise_command = function
  | Structure structure -> Structure (parallelise_structure structure)
  | Statement statement -> Statement statement


and add_parallelised_adjacent_blocks_to_acc adjacent_blocks acc =
  match adjacent_blocks with
  | [] -> acc
  | [ block ] -> acc @ [ Structure (Block_struct block) ]
  | _ -> acc @ parallelise_disjoint_blocks adjacent_blocks


and string_of_block_list block_list =
  Fmt.str
    "[%s]"
    (String.concat
       ~sep:", "
       (List.map ~f:Parallelisation_ast.string_of_block block_list))


and string_of_contents_list contents_list =
  Fmt.str
    "[%s]"
    (String.concat
       ~sep:", "
       (List.map ~f:Parallelisation_ast.string_of_command contents_list))


and string_of_contents_list_cost_tracking contents_list =
  Fmt.str
    "[%s]"
    (String.concat
       ~sep:", "
       (List.map
          ~f:Cost_tracking.Cost_tracking_ast.string_of_command
          contents_list))


and parallelise_block (block : (Cost_tracking.block_runtime_cost, 'b, 'c) block)
  =
  (* print_endline (Cost_tracking.Cost_tracking_ast.string_of_block block); *)
  (* print_endline "\n"; *)
  (* print_endline (string_of_contents_list_cost_tracking block.contents); *)
  let { contents; annotations } = block in
  (* print_endline (string_of_contents_list_cost_tracking contents); *)
  let contents = List.map ~f:parallelise_command contents in
  let parallelised_contents =
    let rec parallelise_contents_inner
        remaining_contents
        passed_adjacent_blocks
        parallelised_contents_acc
      =
      (* print_endline
        (Fmt.str
           "remaining_contents: %s"
           (string_of_contents_list remaining_contents));
      print_endline
        (Fmt.str
           "passed_adjacent_blocks: %s"
           (string_of_block_list passed_adjacent_blocks));
      print_endline
        (Fmt.str
           "parallelised_contents_acc: %s"
           (string_of_contents_list parallelised_contents_acc));
      print_endline "\n\n--\n\n"; *)

      (* print_endline "\n\n------\n\n"; *)
      match remaining_contents with
      | command :: remaining_contents ->
        (* print_endline (Parallelisation_ast.string_of_command command); *)
        (match command with
        | Structure (Block_struct block) ->
          parallelise_contents_inner
            remaining_contents
            (passed_adjacent_blocks @ [ block ])
            parallelised_contents_acc
        | _ ->
          parallelise_contents_inner
            remaining_contents
            []
            (add_parallelised_adjacent_blocks_to_acc
               passed_adjacent_blocks
               parallelised_contents_acc
            @ [ command ]))
      | [] ->
        (* let x = *)
        add_parallelised_adjacent_blocks_to_acc
          passed_adjacent_blocks
          parallelised_contents_acc
      (* in
        print_endline (string_of_contents_list x);
        x *)
    in
    parallelise_contents_inner contents [] []
  in
  (* print_endline
    (Fmt.str
       "final_contents: %s"
       (string_of_contents_list parallelised_contents));
  print_endline "\n\n----\n\n"; *)
  (* print_endline "\n\n-----\n\n"; *)
  { contents = parallelised_contents
  ; annotations =
      { side_effects = annotations.side_effects
      ; scoped_vars = annotations.scoped_vars
      ; runtime_cost = annotations.runtime_cost
      ; parallelise_contents = None
      }
  }


and parallelise_func (func : ('a, 'b, 'c) func) =
  (* print_endline (Cost_tracking.Cost_tracking_ast.string_of_block func.body); *)
  { name = func.name
  ; params = func.params
  ; body = parallelise_block func.body
  ; return_type = func.return_type
  }


let parallelise_program program =
  let parallelised_funcs = List.map ~f:parallelise_func program.funcs in
  { package = program.package
  ; imports =
      (if !contains_parallelisation
      then Import_annotation.add program.imports (Import.create Import.I_Sync)
      else program.imports)
  ; global_vars = program.global_vars
  ; funcs = parallelised_funcs
  }


let pipeline_ast ~debug_file program =
  contains_parallelisation := false;
  let new_program = parallelise_program program in
  (match debug_file with
  | Some debug_file ->
    Parallelisation_ast.output_to_debug_file debug_file new_program
  | None -> ());
  new_program
