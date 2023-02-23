package main



func main ()  {
//{block_type: Default}

x := (0 : UNTYPED)
{
//{block_type: Default}

{
//{block_type: Default}

for i := (0 : UNTYPED);((i : UNTYPED) < (500 : UNTYPED) : UNTYPED);i++ {
//{block_type: Default}

(fmt.Println(("hello world" : UNTYPED)) : UNTYPED)
}
}
}
{
//{block_type: Default}

{
//{block_type: Default}

for i := (0 : UNTYPED);((i : UNTYPED) < (500 : UNTYPED) : UNTYPED);i++ {
//{block_type: Default}

x = ((x : UNTYPED) + (i : UNTYPED) : UNTYPED)
}
}
}
(fmt.Println((x : UNTYPED)) : UNTYPED)
}