package main

import ("fmt")


func alpha_1 () int {
alpha_2 := 0
{for alpha_3 := 0;alpha_3 < 1000;alpha_3++ {
alpha_2 = alpha_2 + alpha_3
}}
return alpha_2
}

func main ()  {
alpha_4 := 0
alpha_5 := 0
alpha_6 := 0
alpha_7 := 0
{alpha_4 = alpha_1()}
{alpha_5 = alpha_1()}
{alpha_6 = alpha_1()}
{alpha_7 = alpha_1()}
fmt.Println(alpha_4 + alpha_5 + alpha_6 + alpha_7)
}