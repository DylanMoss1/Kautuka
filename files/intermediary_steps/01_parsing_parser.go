package main



func main ()  {
//{block_type: Default}

x := (open(("./test_files/a/b.txt" : UNTYPED), user[id:1, ref:1]) : UNTYPED)
y := (open(("./test_files/a/c.txt" : UNTYPED), user[id:1, ref:1]) : UNTYPED)
z := (y : UNTYPED)
a := ("" : UNTYPED)
b := ("" : UNTYPED)
c := ("" : UNTYPED)
{
//{block_type: Default}

a = (read((x : UNTYPED), 100) : UNTYPED)
}
{
//{block_type: Default}

b = (read((y : UNTYPED), 100) : UNTYPED)
}
{
//{block_type: Default}

c = (read((z : UNTYPED), 100) : UNTYPED)
}
(fmt.Println((a : UNTYPED)) : UNTYPED)
(fmt.Println((b : UNTYPED)) : UNTYPED)
(fmt.Println((c : UNTYPED)) : UNTYPED)
}