package main

import ("fmt"
"os"
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



func alpha_1 (alpha_2 string, alpha_3 string) int {
alpha_4 := 0
{for _, alpha_5 := range alpha_2 {
var alpha_5 string = string(alpha_5)
    if alpha_5 == alpha_3 {
alpha_4 = alpha_4 + 1
} 
}}
return alpha_4
}

func alpha_6 (alpha_7 string) int {
var alpha_8 int
var alpha_9 int
var alpha_10 int
var alpha_11 int
var alpha_12 int
{start := time.Now()
{alpha_8 = alpha_1(alpha_7, "a")}
{alpha_9 = alpha_1(alpha_7, "e")}
{alpha_10 = alpha_1(alpha_7, "i")}
{alpha_11 = alpha_1(alpha_7, "o")}
{alpha_12 = alpha_1(alpha_7, "u")}
elapsed := time.Since(start)
print(elapsed.Nanoseconds())}
return alpha_8 + alpha_9 + alpha_10 + alpha_11 + alpha_12
}

func main ()  {
alpha_13 := open("./benchmark/files/large1.txt")
alpha_14 := read(alpha_13)
tempalpha_16 := os.Stdout
os.Stdout = nil
fmt.Println(alpha_6(alpha_14))
os.Stdout = tempalpha_16
}