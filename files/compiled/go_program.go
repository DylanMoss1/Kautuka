package main

import ("fmt")


func main ()  {
alpha_1 := 0
{for alpha_2 := 0;alpha_2 < 1000;alpha_2++ {
alpha_1 = alpha_1 + alpha_2
}}
fmt.Println(alpha_1)
}