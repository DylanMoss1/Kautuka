open Parsed_ast

let rec repeat s n = if n = 0 then "" else s ^ repeat s (n - 1)
let add_indent n s = repeat "\t" n ^ s
let string_of_id = function ID id -> id
let comma_list f xs = String.concat ", " (List.map f xs)
let newline_list f xs n = String.concat (repeat "\n" n) (List.map f xs)

let string_of_type_id = function
  | T_Int -> "int"
  | T_Bool -> "bool"
  | T_String -> "string"

let string_of_value = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | String s -> [%string "\"$(s)\""]

let string_of_package = function
  | Package package -> [%string "package $(string_of_id package)"]

let string_of_var = function
  | VarNonInit (id, type_id) ->
      [%string "var $(string_of_id id) $(string_of_type_id type_id)"]
  | VarInit (id, type_id, value) ->
      [%string
        "var $(string_of_id id) $(string_of_type_id type_id) = \
         $(string_of_value value)"]
  | VarDecl (id, value) ->
      [%string "$(string_of_id id) := $(string_of_value value)"]

let string_of_param = function
  | Param (id, type_id) ->
      [%string "$(string_of_id id) $(string_of_type_id type_id)"]

let string_of_statement indent = function
  | Var var -> add_indent indent (string_of_var var)

let string_of_func indent = function
  | Function (id, params, global_vars) ->
      add_indent indent
        [%string
          "func $(string_of_id id)($(comma_list string_of_param params)) {\n\
           $(newline_list (string_of_statement (indent + 1)) global_vars 1)\n\
           }"]

let string_of_program = function
  | Program (package, global_vars, funcs) ->
      let indent = 0 in
      [%string
        "$(string_of_package package)\n\n\
         $(newline_list string_of_var global_vars 1)\n\n\
         $(newline_list (string_of_func indent) funcs 2)"]
