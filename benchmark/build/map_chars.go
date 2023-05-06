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



func alpha_1 (alpha_2 string) string {
alpha_3 := ""
{for _, alpha_4 := range alpha_2 {
var alpha_4 string = string(alpha_4)
    if alpha_4 != " " {
alpha_3 = alpha_3 + alpha_4
} 
}}
return alpha_3
}

func main ()  {
var alpha_5 string
var alpha_6 string
var alpha_7 string
var alpha_8 string
var alpha_9 string
{start := time.Now()
{alpha_10 := open("./benchmark/files/large1.txt")
alpha_5 = read(alpha_10)}
{alpha_11 := open("./benchmark/files/large2.txt")
alpha_6 = read(alpha_11)}
{alpha_12 := open("./benchmark/files/large3.txt")
alpha_7 = read(alpha_12)}
{alpha_13 := open("./benchmark/files/large4.txt")
alpha_8 = read(alpha_13)}
{alpha_14 := open("./benchmark/files/large5.txt")
alpha_9 = read(alpha_14)}
elapsed := time.Since(start)
print(elapsed.Nanoseconds())}
tempalpha_16 := os.Stdout
os.Stdout = nil
fmt.Println(alpha_5 + alpha_6 + alpha_7 + alpha_8 + alpha_9)
os.Stdout = tempalpha_16
}