package main

import ("fmt"
"os"
"sync"
"time")


func alpha_1 (alpha_2 int, alpha_3 int) bool {
alpha_4 := false
{for alpha_5 := 0;alpha_5 < alpha_2;alpha_5++ {
if (alpha_3 * alpha_5) == alpha_2 {
alpha_4 = true
} 
}}
return alpha_4
}

func alpha_6 (alpha_7 int) int {
alpha_8 := 0
{for alpha_9 := 1;alpha_9 < alpha_7;alpha_9++ {
if alpha_1(alpha_7, alpha_9) {
alpha_8 = alpha_8 + alpha_9
} 
}}
return alpha_8
}

func alpha_10 (alpha_11 int, alpha_12 int) int {
alpha_13 := 0
{for alpha_14 := alpha_11;alpha_14 < alpha_12;alpha_14++ {
alpha_13 = alpha_13 + alpha_6(alpha_14)
}}
return alpha_13
}

func main ()  {
var alpha_15 int
var alpha_16 int
var alpha_17 int
{if 3032444. < 2449218. {start := time.Now()
{alpha_10(1, 50)}
{alpha_10(51, 100)}
{alpha_10(101, 150)}
elapsed := time.Since(start)
print(elapsed.Nanoseconds())} else {start := time.Now()
var wg_alpha_20 sync.WaitGroup
wg_alpha_20.Add(3)
go func(){
{alpha_10(1, 50)}
wg_alpha_20.Done()
}()
go func(){
{alpha_10(51, 100)}
wg_alpha_20.Done()
}()
go func(){
{alpha_10(101, 150)}
wg_alpha_20.Done()
}()
wg_alpha_20.Wait()
elapsed := time.Since(start)
print(elapsed.Nanoseconds())}}
tempalpha_21 := os.Stdout
os.Stdout = nil
fmt.Println(alpha_15 + alpha_16 + alpha_17)
os.Stdout = tempalpha_21
}