package main

import ("fmt"
"os"
"sync"
"time")
func check(err error) {
	if err != nil {
			panic(err)
	}
}

func open(filename string) *os.File {
file, err := os.OpenFile(filename, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0666)
check(err)
return file
}

func read(file *os.File) string {
  dat, err := os.ReadFile(file.Name())
  check(err)
  return string(dat)
}



func alpha_1 (alpha_2 string, alpha_3 string, alpha_4 string) string {
alpha_5 := ""
{for _, alpha_6 := range alpha_2 {
var alpha_6 string = string(alpha_6)
    if alpha_6 != alpha_3 {
alpha_5 = alpha_5 + alpha_6
} else {alpha_5 = alpha_5 + alpha_4}
}}
return alpha_5
}

func main ()  {
var alpha_7 string
var alpha_8 string
var alpha_9 string
alpha_10 := read(open("./benchmark/files/very_small1.txt"))
alpha_11 := read(open("./benchmark/files/very_small2.txt"))
alpha_12 := read(open("./benchmark/files/very_small2.txt"))
{if 1350012. < 676452. {start := time.Now()
{alpha_7 = alpha_1(alpha_10, "a", "b")}
{alpha_8 = alpha_1(alpha_11, "c", "d")}
{alpha_9 = alpha_1(alpha_12, "e", "f")}
elapsed := time.Since(start)
print(elapsed.Nanoseconds())} else {start := time.Now()
var wg_alpha_17 sync.WaitGroup
wg_alpha_17.Add(3)
go func(){
{alpha_7 = alpha_1(alpha_10, "a", "b")}
wg_alpha_17.Done()
}()
go func(){
{alpha_8 = alpha_1(alpha_11, "c", "d")}
wg_alpha_17.Done()
}()
go func(){
{alpha_9 = alpha_1(alpha_12, "e", "f")}
wg_alpha_17.Done()
}()
wg_alpha_17.Wait()
elapsed := time.Since(start)
print(elapsed.Nanoseconds())}}
tempalpha_18 := os.Stdout
os.Stdout = nil
fmt.Println(alpha_7)
os.Stdout = tempalpha_18
tempalpha_19 := os.Stdout
os.Stdout = nil
fmt.Println(alpha_8)
os.Stdout = tempalpha_19
tempalpha_20 := os.Stdout
os.Stdout = nil
fmt.Println(alpha_9)
os.Stdout = tempalpha_20
}