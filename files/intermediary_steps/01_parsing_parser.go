package main



func main ()  {
//{block_type: Default}

total := (0 : UNTYPED)
{
//{block_type: Default}

for i := (0 : UNTYPED);((i : UNTYPED) < (1000 : UNTYPED) : UNTYPED);i++ {
//{block_type: Default}

total = ((total : UNTYPED) + (i : UNTYPED) : UNTYPED)
}
}
(fmt.Println((total : UNTYPED)) : UNTYPED)
}