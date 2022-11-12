open Parsed_ast

type program = {
  package: package;
  vars: var list; 
  funcs: func list; 
}

let parsed_ast_to_ast_types = function
  | Program(package, vars, funcs) -> 
    {
      package = package;
      vars = vars;
      funcs = funcs;
    }