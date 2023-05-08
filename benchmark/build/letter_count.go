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



func alpha_1 (alpha_2 string) int {
alpha_3 := 0
{for _, alpha_4 := range alpha_2 {
var alpha_4 string = string(alpha_4)
    if alpha_4 != " " {
alpha_3 = alpha_3 + 1
} 
}}
return alpha_3
}

func main ()  {
var alpha_5 int
var alpha_6 int
var alpha_7 int
{if 4302768. < 2152830. {start := time.Now()
{alpha_5 = alpha_1(read(open("./benchmark/files/large1.txt")))}
{alpha_6 = alpha_1(read(open("./benchmark/files/large2.txt")))}
{alpha_7 = alpha_1(read(open("./benchmark/files/large3.txt")))}
elapsed := time.Since(start)
print(elapsed.Nanoseconds())} else {start := time.Now()
var wg_alpha_10 sync.WaitGroup
wg_alpha_10.Add(3)
go func(){
{alpha_5 = alpha_1(read(open("./benchmark/files/large1.txt")))}
wg_alpha_10.Done()
}()
go func(){
{alpha_6 = alpha_1(read(open("./benchmark/files/large2.txt")))}
wg_alpha_10.Done()
}()
go func(){
{alpha_7 = alpha_1(read(open("./benchmark/files/large3.txt")))}
wg_alpha_10.Done()
}()
wg_alpha_10.Wait()
elapsed := time.Since(start)
print(elapsed.Nanoseconds())}}
tempalpha_11 := os.Stdout
os.Stdout = nil
fmt.Println(alpha_5 + alpha_6 + alpha_7)
os.Stdout = tempalpha_11
}