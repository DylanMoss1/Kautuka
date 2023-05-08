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



func alpha_1 (alpha_2 string) string {
alpha_3 := ""
{for _, alpha_4 := range alpha_2 {
var alpha_4 string = string(alpha_4)
    var alpha_5 string
var alpha_6 string
var alpha_7 string
{if 0. < 1446. {start := time.Now()
{if (alpha_4 != " ") && (alpha_4 != "-") {
alpha_5 = alpha_4
} else {alpha_5 = ""}}
{if alpha_4 == " " {
alpha_6 = "_"
} else {alpha_6 = ""}}
{if alpha_4 == "-" {
alpha_7 = "-"
} else {alpha_7 = ""}}
elapsed := time.Since(start)
print(elapsed.Nanoseconds())} else {start := time.Now()
var wg_alpha_21 sync.WaitGroup
wg_alpha_21.Add(3)
go func(){
{if (alpha_4 != " ") && (alpha_4 != "-") {
alpha_5 = alpha_4
} else {alpha_5 = ""}}
wg_alpha_21.Done()
}()
go func(){
{if alpha_4 == " " {
alpha_6 = "_"
} else {alpha_6 = ""}}
wg_alpha_21.Done()
}()
go func(){
{if alpha_4 == "-" {
alpha_7 = "-"
} else {alpha_7 = ""}}
wg_alpha_21.Done()
}()
wg_alpha_21.Wait()
elapsed := time.Since(start)
print(elapsed.Nanoseconds())}}
alpha_3 = alpha_3 + alpha_5 + alpha_6 + alpha_7
}}
return alpha_3
}

func main ()  {
var alpha_8 string
var alpha_9 string
var alpha_10 string
var alpha_11 string
var alpha_12 string
{if 4601565. < 1382430. {start := time.Now()
{alpha_13 := open("./benchmark/files/very_small1.txt")
alpha_8 = alpha_1(read(alpha_13))}
{alpha_14 := open("./benchmark/files/very_small2.txt")
alpha_9 = alpha_1(read(alpha_14))}
{alpha_15 := open("./benchmark/files/very_small3.txt")
alpha_10 = alpha_1(read(alpha_15))}
{alpha_16 := open("./benchmark/files/very_small4.txt")
alpha_11 = alpha_1(read(alpha_16))}
{alpha_17 := open("./benchmark/files/very_small5.txt")
alpha_12 = alpha_1(read(alpha_17))}
elapsed := time.Since(start)
print(elapsed.Nanoseconds())} else {start := time.Now()
var wg_alpha_22 sync.WaitGroup
wg_alpha_22.Add(5)
go func(){
{alpha_13 := open("./benchmark/files/very_small1.txt")
alpha_8 = alpha_1(read(alpha_13))}
wg_alpha_22.Done()
}()
go func(){
{alpha_14 := open("./benchmark/files/very_small2.txt")
alpha_9 = alpha_1(read(alpha_14))}
wg_alpha_22.Done()
}()
go func(){
{alpha_15 := open("./benchmark/files/very_small3.txt")
alpha_10 = alpha_1(read(alpha_15))}
wg_alpha_22.Done()
}()
go func(){
{alpha_16 := open("./benchmark/files/very_small4.txt")
alpha_11 = alpha_1(read(alpha_16))}
wg_alpha_22.Done()
}()
go func(){
{alpha_17 := open("./benchmark/files/very_small5.txt")
alpha_12 = alpha_1(read(alpha_17))}
wg_alpha_22.Done()
}()
wg_alpha_22.Wait()
elapsed := time.Since(start)
print(elapsed.Nanoseconds())}}
tempalpha_23 := os.Stdout
os.Stdout = nil
fmt.Println(alpha_8 + alpha_9 + alpha_10 + alpha_11 + alpha_12)
os.Stdout = tempalpha_23
}