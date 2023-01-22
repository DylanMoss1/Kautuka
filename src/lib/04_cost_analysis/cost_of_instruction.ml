open! Core


let tr = 1

let cost_of_not = tr
let cost_of_u_minus = tr
let cost_of_plus_int = tr 
let cost_of_plus_str = tr 
let cost_of_b_minus = tr 
let cost_of_mult = tr 
let cost_of_lt = tr 
let cost_of_le = tr 
let cost_of_gt = tr 
let cost_of_ge = tr 
let cost_of_eq = tr 
let cost_of_ne = tr 
let cost_of_and = tr 
let cost_of_or = tr 
let cost_of_var_read = tr 
let cost_of_var_non_init = tr 
let cost_of_var_init = tr 
let cost_of_var_decl = tr 
let cost_of_var_assign = tr 
let cost_of_post_inc = tr 
let cost_of_post_dec = tr 
let cost_of_user_func_call = 50
let cost_of_print x = 20 * x
let cost_of_input = 5000
let cost_of_open = 300
let cost_of_read = 300
let cost_of_write x = 500 + x
let cost_of_append x = 500 + x
let cost_of_for_loop = 20
let cost_of_for_each = 30
let cost_of_if_record = 10
