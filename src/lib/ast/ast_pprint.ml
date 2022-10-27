open Ast_structure

let rec repeat s n = if n = 0 then "" else s ^ repeat s (n - 1)
let add_indent n s = repeat "\t" n ^ s
let comma_list f xs = String.concat ", " (List.map f xs)
let newline_list f xs n = String.concat (repeat "\n" n) (List.map f xs)

let string_of_id indent = function ID id -> add_indent indent id

let string_of_type_id indent = function
  | T_Int -> add_indent indent "int"
  | T_Bool -> add_indent indent "bool"
  | T_String -> add_indent indent "string"

let string_of_value indent = function
  | Int i -> add_indent indent (string_of_int i)
  | Bool b -> add_indent indent (string_of_bool b)
  | String s -> add_indent indent [%string "\"$(s)\""]

let string_of_package indent = function
  | Package package -> add_indent indent [%string "package $((string_of_id indent) package)"] 

let string_of_global_var indent = function
  | GlobalVar (id, type_id) ->
    add_indent indent [%string "var $(string_of_id indent id) $((string_of_type_id indent) type_id)"]
  | GlobalVarInit (id, type_id, value) ->
    add_indent indent [%string
        "var $(string_of_id indent id) $((string_of_type_id indent) type_id) = \
         $((string_of_value indent) value)"]

let string_of_param indent = function
  | Param (id, type_id) ->
    add_indent indent [%string "$(string_of_id indent id) $((string_of_type_id indent) type_id)"]

let string_of_func indent = function
  | Function (id, params, global_vars) ->
      add_indent indent
        [%string
          "func $(string_of_id indent id)($(comma_list (string_of_param indent) params)) {\n\
           $(newline_list (string_of_global_var (indent + 1)) global_vars 1)\n\
           }"]
        

let string_of_program = function
  | Program (package, global_vars, funcs) ->
    let indent = 0 in
    add_indent 0 
        [%string
          "$(string_of_package indent package)\n\n\
           $(newline_list (string_of_global_var indent) global_vars 1)\n\n\
           $(newline_list (string_of_func indent) funcs 2)"]
        
