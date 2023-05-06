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



func main ()  {
var alpha_1 string
var alpha_2 string
var alpha_3 string
{start := time.Now()
{alpha_4 := open("./benchmark/files/very_large1.txt")
alpha_1 = read(alpha_4)}
{alpha_5 := open("./benchmark/files/very_large2.txt")
alpha_2 = read(alpha_5)}
{alpha_6 := open("./benchmark/files/very_large3.txt")
alpha_3 = read(alpha_6)}
elapsed := time.Since(start)
print(elapsed.Nanoseconds())}
tempalpha_8 := os.Stdout
os.Stdout = nil
fmt.Println(alpha_1 + alpha_2 + alpha_3)
os.Stdout = tempalpha_8
}