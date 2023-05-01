package main

import ("os")


func main ()  {
//{block_type: Default}

var file1_contents string
var file2_contents string
var file3_contents string
var file4_contents string
var file5_contents string
{
//{block_type: Default}

file1 := (open(("./benchmark/files/read1.txt" : UNTYPED), gen[id:1, ref:1]) : UNTYPED)
file1_contents = (read((file1 : UNTYPED), 20000) : UNTYPED)
}
{
//{block_type: Default}

file2 := (open(("./benchmark/files/read2.txt" : UNTYPED), gen[id:1, ref:1]) : UNTYPED)
file2_contents = (read((file2 : UNTYPED), 20000) : UNTYPED)
}
{
//{block_type: Default}

file3 := (open(("./benchmark/files/read3.txt" : UNTYPED), gen[id:1, ref:1]) : UNTYPED)
file3_contents = (read((file3 : UNTYPED), 20000) : UNTYPED)
}
{
//{block_type: Default}

file4 := (open(("./benchmark/files/read4.txt" : UNTYPED), gen[id:1, ref:1]) : UNTYPED)
file4_contents = (read((file4 : UNTYPED), 20000) : UNTYPED)
}
{
//{block_type: Default}

file5 := (open(("./benchmark/files/read5.txt" : UNTYPED), gen[id:1, ref:1]) : UNTYPED)
file5_contents = (read((file5 : UNTYPED), 20000) : UNTYPED)
}
_ = (((((file1_contents : UNTYPED) + (file2_contents : UNTYPED) : UNTYPED) + (file3_contents : UNTYPED) : UNTYPED) + (file4_contents : UNTYPED) : UNTYPED) + (file5_contents : UNTYPED) : UNTYPED)
}