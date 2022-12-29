open! Core

let cost_of_unop = 1
let cost_of_binop = 2
let cost_of_var_statement = 5
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
