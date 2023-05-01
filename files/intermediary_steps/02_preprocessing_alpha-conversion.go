package main

import ("os")


func var{name: main, alpha: main} ()  {
//{block_type: Default, scoped_vars: {}}

var var{name: file1_contents, alpha: alpha_1} string
var var{name: file2_contents, alpha: alpha_2} string
var var{name: file3_contents, alpha: alpha_3} string
var var{name: file4_contents, alpha: alpha_4} string
var var{name: file5_contents, alpha: alpha_5} string
{
//{block_type: Default, scoped_vars: {alpha_5, alpha_4, alpha_3, alpha_2, alpha_1}}

var{name: file1, alpha: alpha_6} := (open(("./benchmark/files/read1.txt" : UNTYPED), gen[id:1, ref:1]) : UNTYPED)
var{name: file1_contents, alpha: alpha_1} = (read((var{name: file1, alpha: alpha_6} : UNTYPED), 20000) : UNTYPED)
}
{
//{block_type: Default, scoped_vars: {alpha_5, alpha_4, alpha_3, alpha_2, alpha_1}}

var{name: file2, alpha: alpha_7} := (open(("./benchmark/files/read2.txt" : UNTYPED), gen[id:1, ref:1]) : UNTYPED)
var{name: file2_contents, alpha: alpha_2} = (read((var{name: file2, alpha: alpha_7} : UNTYPED), 20000) : UNTYPED)
}
{
//{block_type: Default, scoped_vars: {alpha_5, alpha_4, alpha_3, alpha_2, alpha_1}}

var{name: file3, alpha: alpha_8} := (open(("./benchmark/files/read3.txt" : UNTYPED), gen[id:1, ref:1]) : UNTYPED)
var{name: file3_contents, alpha: alpha_3} = (read((var{name: file3, alpha: alpha_8} : UNTYPED), 20000) : UNTYPED)
}
{
//{block_type: Default, scoped_vars: {alpha_5, alpha_4, alpha_3, alpha_2, alpha_1}}

var{name: file4, alpha: alpha_9} := (open(("./benchmark/files/read4.txt" : UNTYPED), gen[id:1, ref:1]) : UNTYPED)
var{name: file4_contents, alpha: alpha_4} = (read((var{name: file4, alpha: alpha_9} : UNTYPED), 20000) : UNTYPED)
}
{
//{block_type: Default, scoped_vars: {alpha_5, alpha_4, alpha_3, alpha_2, alpha_1}}

var{name: file5, alpha: alpha_10} := (open(("./benchmark/files/read5.txt" : UNTYPED), gen[id:1, ref:1]) : UNTYPED)
var{name: file5_contents, alpha: alpha_5} = (read((var{name: file5, alpha: alpha_10} : UNTYPED), 20000) : UNTYPED)
}
var{name: _, alpha: alpha_11} = (((((var{name: file1_contents, alpha: alpha_1} : UNTYPED) + (var{name: file2_contents, alpha: alpha_2} : UNTYPED) : UNTYPED) + (var{name: file3_contents, alpha: alpha_3} : UNTYPED) : UNTYPED) + (var{name: file4_contents, alpha: alpha_4} : UNTYPED) : UNTYPED) + (var{name: file5_contents, alpha: alpha_5} : UNTYPED) : UNTYPED)
}