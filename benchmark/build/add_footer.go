package main

import ("os"
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

func append(file *os.File, contents string) {
  _, err := file.WriteString(contents)
  check(err)
  file.Sync()
}



func alpha_1 (alpha_2 int) string {
alpha_3 := false
alpha_4 := false
alpha_5 := false
{if 0. < 1446. {start := time.Now()
{if alpha_2 == 1 {
alpha_3 = true
} }
{if alpha_2 == 100 {
alpha_4 = true
} }
{if alpha_2 == 10000 {
alpha_5 = true
} }
elapsed := time.Since(start)
print(elapsed.Nanoseconds())} else {start := time.Now()
var wg_alpha_14 sync.WaitGroup
wg_alpha_14.Add(3)
go func(){
{if alpha_2 == 1 {
alpha_3 = true
} }
wg_alpha_14.Done()
}()
go func(){
{if alpha_2 == 100 {
alpha_4 = true
} }
wg_alpha_14.Done()
}()
go func(){
{if alpha_2 == 10000 {
alpha_5 = true
} }
wg_alpha_14.Done()
}()
wg_alpha_14.Wait()
elapsed := time.Since(start)
print(elapsed.Nanoseconds())}}
if alpha_3 || alpha_4 || alpha_5 {
return "TRUE"
} else {return "FALSE"}
}

func main ()  {
alpha_6 := open("./benchmark/files/tmp/small1.txt")
alpha_7 := open("./benchmark/files/tmp/small2.txt")
alpha_8 := open("./benchmark/files/tmp/small3.txt")
{if 3932256. < 1967574.5 {start := time.Now()
{alpha_9 := alpha_1(100)
append(alpha_6, alpha_9)}
{alpha_10 := alpha_1(200)
append(alpha_7, alpha_10)}
{alpha_11 := alpha_1(300)
append(alpha_8, alpha_11)}
elapsed := time.Since(start)
print(elapsed.Nanoseconds())} else {start := time.Now()
var wg_alpha_15 sync.WaitGroup
wg_alpha_15.Add(3)
go func(){
{alpha_9 := alpha_1(100)
append(alpha_6, alpha_9)}
wg_alpha_15.Done()
}()
go func(){
{alpha_10 := alpha_1(200)
append(alpha_7, alpha_10)}
wg_alpha_15.Done()
}()
go func(){
{alpha_11 := alpha_1(300)
append(alpha_8, alpha_11)}
wg_alpha_15.Done()
}()
wg_alpha_15.Wait()
elapsed := time.Since(start)
print(elapsed.Nanoseconds())}}
}