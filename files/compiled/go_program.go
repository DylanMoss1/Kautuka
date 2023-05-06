package main

import ("fmt"
"time")


func main ()  {
tempalpha_2 := os.Stdout
os.Stdout = nil
fmt.Println("program working")
os.Stdout = tempalpha_2
}