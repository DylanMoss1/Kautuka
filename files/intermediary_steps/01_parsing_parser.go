package main



func generate_footer (x int) string {
//{block_type: Default}

footer1 := (false : UNTYPED)
footer2 := (false : UNTYPED)
footer3 := (false : UNTYPED)
{
//{block_type: Default}

if ((x : UNTYPED) == (1 : UNTYPED) : UNTYPED) {
//{block_type: Default}

footer1 = (true : UNTYPED)
} 
}
{
//{block_type: Default}

if ((x : UNTYPED) == (100 : UNTYPED) : UNTYPED) {
//{block_type: Default}

footer2 = (true : UNTYPED)
} 
}
{
//{block_type: Default}

if ((x : UNTYPED) == (10000 : UNTYPED) : UNTYPED) {
//{block_type: Default}

footer3 = (true : UNTYPED)
} 
}
if (((footer1 : UNTYPED) || (footer2 : UNTYPED) : UNTYPED) || (footer3 : UNTYPED) : UNTYPED) {
//{block_type: Default}

return ("TRUE" : UNTYPED)
} //{block_type: Default}

return ("FALSE" : UNTYPED)
}
func main ()  {
//{block_type: Default}

file1 := (open(("./benchmark/files/tmp/small1.txt" : UNTYPED), user[id:1, user_ref:1]) : UNTYPED)
file2 := (open(("./benchmark/files/tmp/small2.txt" : UNTYPED), user[id:2, user_ref:1]) : UNTYPED)
file3 := (open(("./benchmark/files/tmp/small3.txt" : UNTYPED), user[id:3, user_ref:1]) : UNTYPED)
{
//{block_type: Default}

footer := (generate_footer((100 : UNTYPED)) : UNTYPED)
(append((file1 : UNTYPED), (footer : UNTYPED)) : UNTYPED)
}
{
//{block_type: Default}

footer := (generate_footer((200 : UNTYPED)) : UNTYPED)
(append((file2 : UNTYPED), (footer : UNTYPED)) : UNTYPED)
}
{
//{block_type: Default}

footer := (generate_footer((300 : UNTYPED)) : UNTYPED)
(append((file3 : UNTYPED), (footer : UNTYPED)) : UNTYPED)
}
}