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



func main ()  {
alpha_1 := open("./benchmark/files/tmp/small1.txt")
alpha_2 := open("./benchmark/files/tmp/small2.txt")
alpha_3 := open("./benchmark/files/tmp/small3.txt")
alpha_4 := "FOOTER"
{if 3932268. < 1967580. {start := time.Now()
{append(alpha_1, alpha_4)}
{append(alpha_2, alpha_4)}
{append(alpha_3, alpha_4)}
elapsed := time.Since(start)
print(elapsed.Nanoseconds())} else {start := time.Now()
var wg_alpha_6 sync.WaitGroup
wg_alpha_6.Add(3)
go func(){
{append(alpha_1, alpha_4)}
wg_alpha_6.Done()
}()
go func(){
{append(alpha_2, alpha_4)}
wg_alpha_6.Done()
}()
go func(){
{append(alpha_3, alpha_4)}
wg_alpha_6.Done()
}()
wg_alpha_6.Wait()
elapsed := time.Since(start)
print(elapsed.Nanoseconds())}}
}