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
    alpha_3 = alpha_3 + alpha_4
alpha_3 = alpha_3 + " "
}}
return alpha_3
}

func main ()  {
var alpha_5 string
var alpha_6 string
var alpha_7 string
{start := time.Now()
{alpha_5 = alpha_1(read(open("./benchmark/files/very_small1.txt")))}
{alpha_6 = alpha_1(read(open("./benchmark/files/very_small2.txt")))}
{alpha_7 = alpha_1(read(open("./benchmark/files/very_small3.txt")))}
elapsed := time.Since(start)
print(elapsed.Nanoseconds())}
tempalpha_11 := os.Stdout
os.Stdout = nil
fmt.Println(alpha_5)
os.Stdout = tempalpha_11
tempalpha_12 := os.Stdout
os.Stdout = nil
fmt.Println(alpha_6)
os.Stdout = tempalpha_12
tempalpha_13 := os.Stdout
os.Stdout = nil
fmt.Println(alpha_7)
os.Stdout = tempalpha_13
}