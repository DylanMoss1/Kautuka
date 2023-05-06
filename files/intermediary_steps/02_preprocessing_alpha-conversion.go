package main

import ("fmt"
"os")


func var{name: map_chars, alpha: alpha_1} (var{name: contents, alpha: alpha_2} string, var{name: from, alpha: alpha_3} string, var{name: to, alpha: alpha_4} string) string {
//{block_type: Default, scoped_vars: {alpha_4, alpha_3, alpha_2}}

var{name: result, alpha: alpha_5} := ("" : UNTYPED)
{
//{block_type: Default, scoped_vars: {alpha_5, alpha_4, alpha_3, alpha_2}}

for var{name: char, alpha: alpha_6} := range (var{name: contents, alpha: alpha_2} : UNTYPED) {
//{block_type: Default, scoped_vars: {alpha_6, alpha_5, alpha_4, alpha_3, alpha_2}}

if ((var{name: char, alpha: alpha_6} : UNTYPED) == (var{name: from, alpha: alpha_3} : UNTYPED) : UNTYPED) {
//{block_type: Default, scoped_vars: {alpha_6, alpha_5, alpha_4, alpha_3, alpha_2}}

var{name: result, alpha: alpha_5} = ((var{name: result, alpha: alpha_5} : UNTYPED) + (var{name: to, alpha: alpha_4} : UNTYPED) : UNTYPED)
} //{block_type: Default, scoped_vars: {alpha_6, alpha_5, alpha_4, alpha_3, alpha_2}}

var{name: result, alpha: alpha_5} = ((var{name: result, alpha: alpha_5} : UNTYPED) + (var{name: char, alpha: alpha_6} : UNTYPED) : UNTYPED)
}
}
return (var{name: result, alpha: alpha_5} : UNTYPED)
}
func var{name: main, alpha: main} ()  {
//{block_type: Default, scoped_vars: {alpha_1}}

var var{name: result1, alpha: alpha_7} string
var var{name: result2, alpha: alpha_8} string
var var{name: result3, alpha: alpha_9} string
{
//{block_type: Default, scoped_vars: {alpha_9, alpha_8, alpha_7, alpha_1}}

var{name: result1, alpha: alpha_7} = (var{name: map_chars, alpha: alpha_1}((read((open(("./benchmark/files/very_small1.txt" : UNTYPED), user[id:1, user_ref:1]) : UNTYPED), 20000) : UNTYPED), ("a" : UNTYPED), ("b" : UNTYPED)) : UNTYPED)
}
{
//{block_type: Default, scoped_vars: {alpha_9, alpha_8, alpha_7, alpha_1}}

var{name: result2, alpha: alpha_8} = (var{name: map_chars, alpha: alpha_1}((read((open(("./benchmark/files/very_small2.txt" : UNTYPED), user[id:2, user_ref:1]) : UNTYPED), 20000) : UNTYPED), ("c" : UNTYPED), ("d" : UNTYPED)) : UNTYPED)
}
{
//{block_type: Default, scoped_vars: {alpha_9, alpha_8, alpha_7, alpha_1}}

var{name: result3, alpha: alpha_9} = (var{name: map_chars, alpha: alpha_1}((read((open(("./benchmark/files/very_small3.txt" : UNTYPED), user[id:3, user_ref:1]) : UNTYPED), 20000) : UNTYPED), ("e" : UNTYPED), ("f" : UNTYPED)) : UNTYPED)
}
(fmt.Println((var{name: result1, alpha: alpha_7} : UNTYPED)) : UNTYPED)
(fmt.Println((var{name: result2, alpha: alpha_8} : UNTYPED)) : UNTYPED)
(fmt.Println((var{name: result3, alpha: alpha_9} : UNTYPED)) : UNTYPED)
}