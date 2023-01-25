open! Core
open Ast.Ast_types
open Side_effect.Variable_id
open Side_effect.Side_effect_tracking
open Preperation.Import
open Cost_analysis.Types_cost
open Ast.Annotated_ast
open Parsing
open Cost_analysis.Cost

(* 
   
WE WANT TO REMOVE BLOCK_TYPE FROM THIS AT SOME POINT 

*)
module Block_parallelised_annotation = struct
  type t =
    { block_type : Parser_types.block_type
    ; side_effects : Side_effect_set.t
    ; cost_term : Cost.t
    ; parallelise_contents : int option
    }

  let string_of_parallelise_contents = function
    | Some i -> Fmt.str "Some %d" i
    | None -> "None"


  let string_of_t t =
    Fmt.str
      "[%s, %s, %s, %s]"
      (Parser_types.string_of_block_type t.block_type)
      (Side_effect_set.string_of_t t.side_effects)
      (Cost.string_of_t t.cost_term)
      (string_of_parallelise_contents t.parallelise_contents)
end

module Parallelisation_ast =
  Make_annotated_ast (Block_parallelised_annotation) (Alpha_var_annotation)
    (Import_annotation)
    (Expr_type_cost_annotation)

let rec disjoint_block_collection_single_pass
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
    let partial_apply_collect_blocks =
      disjoint_block_collection_single_pass block_list new_collected_effects
    in
    if Side_effect_set.disjoint block.annotations.side_effects collected_effects
    then partial_apply_collect_blocks (block :: collected_blocks) passed_blocks
    else partial_apply_collect_blocks collected_blocks (block :: passed_blocks)
  | [] -> List.rev collected_blocks, List.rev passed_blocks


let disjoint_block_collection
    (block_list : (Parallelisation_ast.block_annot, 'b, 'c) block list)
  =
  let rec disjoint_block_collection_inner block_list thread_list =
    if List.length block_list = 0
    then thread_list
    else (
      let collected_blocks, remaining_blocks =
        disjoint_block_collection_single_pass
          block_list
          Side_effect_set.empty
          []
          []
      in
      disjoint_block_collection_inner
        remaining_blocks
        (List.append thread_list [ collected_blocks ]))
  in
  disjoint_block_collection_inner block_list []


let parallelise_disjoint_blocks
    (block_list : (Parallelisation_ast.block_annot, 'b, 'c) block list)
  =
  let disjoint_blocks_list = disjoint_block_collection block_list in
  List.map
    ~f:(fun disjoint_blocks ->
      Structure
        (Block_struct
           { contents =
               List.map
                 ~f:(fun block -> Structure (Block_struct block))
                 disjoint_blocks
           ; annotations =
               { block_type = Default
               ; side_effects =
                   List.fold_left
                     ~init:Side_effect_set.empty
                     ~f:(fun acc block ->
                       Side_effect_set.union acc block.annotations.side_effects)
                     disjoint_blocks
               ; cost_term =
                   List.fold_left
                     ~init:Cost.zero
                     ~f:(fun acc block ->
                       Cost.sum acc block.annotations.cost_term)
                     disjoint_blocks
               ; parallelise_contents = Some (List.length disjoint_blocks)
               }
           }))
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


and join_block_list_to_acc acc block_list =
  match block_list with
  | [] -> acc
  | [ block ] -> acc @ [ Structure (Block_struct block) ]
  | _ -> acc @ parallelise_disjoint_blocks block_list


and parallelise_block block : (Parallelisation_ast.block_annot, 'c, 'd) block =
  let { contents; annotations } = block in
  let inner_parallelised_contents = List.map ~f:parallelise_command contents in
  let parallelised_contents =
    let rec parallelise_contents command_list block_acc command_acc =
      match command_list with
      | command :: command_list ->
        (match command with
        | Structure (Block_struct block) ->
          parallelise_contents command_list (block_acc @ [ block ]) command_acc
        | _ ->
          parallelise_contents
            command_list
            []
            (join_block_list_to_acc command_acc block_acc)
          @ [ command ])
      | [] -> join_block_list_to_acc command_acc block_acc
    in
    parallelise_contents inner_parallelised_contents [] []
  in
  { contents = parallelised_contents; annotations }


and parallelise_func (func : ('a, 'b, 'c) func)
    : (Parallelisation_ast.block_annot, 'd, 'e) func
  =
  { name = func.name
  ; params = func.params
  ; body = parallelise_block func.body
  ; return_type = func.return_type
  }


let parallelise_program program
    : (Parallelisation_ast.block_annot, 'b, 'c, 'd) program
  =
  { package = program.package
  ; imports = program.imports
  ; global_vars = program.global_vars
  ; funcs = List.map ~f:parallelise_func program.funcs
  }
