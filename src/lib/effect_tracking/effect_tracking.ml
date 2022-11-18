type effect = {
  effects: effects; 
  effect_type: effect_type; 
}

type effects = 
| Console_IO 
| File_IO 
| Var_mutation of var 

type effect_type = 
| READ | WRITE 