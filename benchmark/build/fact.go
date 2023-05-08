package main

import ("fmt"
"os"
"sync"
"time")


func alpha_1 (alpha_2 int) int {
alpha_3 := 1
{for alpha_4 := 1;alpha_4 < alpha_2;alpha_4++ {
alpha_3 = alpha_3 * alpha_4
}}
return alpha_3
}

func alpha_5 (alpha_6 int, alpha_7 int) int {
alpha_8 := 0
{for alpha_9 := alpha_6;alpha_9 < alpha_7;alpha_9++ {
alpha_8 = alpha_8 + alpha_1(alpha_9)
}}
return alpha_8
}

func main ()  {
alpha_10 := 0
alpha_11 := 0
alpha_12 := 0
{if 10989503. < 6744697.5 {start := time.Now()
{alpha_10 = alpha_10 + alpha_5(1, 1000)}
{alpha_11 = alpha_11 + alpha_5(1001, 2000)}
{alpha_12 = alpha_12 + alpha_5(2001, 3000)}
elapsed := time.Since(start)
print(elapsed.Nanoseconds())} else {start := time.Now()
var wg_alpha_15 sync.WaitGroup
wg_alpha_15.Add(3)
go func(){
{alpha_10 = alpha_10 + alpha_5(1, 1000)}
wg_alpha_15.Done()
}()
go func(){
{alpha_11 = alpha_11 + alpha_5(1001, 2000)}
wg_alpha_15.Done()
}()
go func(){
{alpha_12 = alpha_12 + alpha_5(2001, 3000)}
wg_alpha_15.Done()
}()
wg_alpha_15.Wait()
elapsed := time.Since(start)
print(elapsed.Nanoseconds())}}
tempalpha_16 := os.Stdout
os.Stdout = nil
fmt.Println(alpha_10 + alpha_11 + alpha_12)
os.Stdout = tempalpha_16
}