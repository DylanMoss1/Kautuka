package main

import ("fmt")


func main ()  {
alpha_1 := 0
{{{for alpha_2 := 0;alpha_2 < 500;alpha_2++ {
fmt.Println("hello world")
}}}
{{for alpha_3 := 0;alpha_3 < 500;alpha_3++ {
alpha_1 = alpha_1 + alpha_3
}}}}
fmt.Println(alpha_1)
}