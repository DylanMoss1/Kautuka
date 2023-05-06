package main

import ("fmt"
"os")


func map_chars (contents string, from string, to string) string {
//{block_type: Default}

result := ("" : UNTYPED)
{
//{block_type: Default}

for char := range (contents : UNTYPED) {
//{block_type: Default}

if ((char : UNTYPED) == (from : UNTYPED) : UNTYPED) {
//{block_type: Default}

result = ((result : UNTYPED) + (to : UNTYPED) : UNTYPED)
} //{block_type: Default}

result = ((result : UNTYPED) + (char : UNTYPED) : UNTYPED)
}
}
return (result : UNTYPED)
}
func main ()  {
//{block_type: Default}

var result1 string
var result2 string
var result3 string
{
//{block_type: Default}

result1 = (map_chars((read((open(("./benchmark/files/very_small1.txt" : UNTYPED), user[id:1, user_ref:1]) : UNTYPED), 20000) : UNTYPED), ("a" : UNTYPED), ("b" : UNTYPED)) : UNTYPED)
}
{
//{block_type: Default}

result2 = (map_chars((read((open(("./benchmark/files/very_small2.txt" : UNTYPED), user[id:2, user_ref:1]) : UNTYPED), 20000) : UNTYPED), ("c" : UNTYPED), ("d" : UNTYPED)) : UNTYPED)
}
{
//{block_type: Default}

result3 = (map_chars((read((open(("./benchmark/files/very_small3.txt" : UNTYPED), user[id:3, user_ref:1]) : UNTYPED), 20000) : UNTYPED), ("e" : UNTYPED), ("f" : UNTYPED)) : UNTYPED)
}
(fmt.Println((result1 : UNTYPED)) : UNTYPED)
(fmt.Println((result2 : UNTYPED)) : UNTYPED)
(fmt.Println((result3 : UNTYPED)) : UNTYPED)
}