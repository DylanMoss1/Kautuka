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

func read(file *os.File) string {
  dat, err := os.ReadFile(file.Name())
  check(err)
  return string(dat)
}

func write(file *os.File, contents string) {
  file, err := os.Create(file.Name())
  check(err) 
  _, err = file.WriteString(contents)
  check(err)
  file.Sync()
}



func main ()  {
alpha_1 := open("./benchmark/files/tmp/small1.txt")
alpha_2 := open("./benchmark/files/tmp/small2.txt")
alpha_3 := open("./benchmark/files/tmp/small3.txt")
alpha_4 := read(open("./benchmark/files/tmp/tmp1.txt"))
alpha_5 := read(open("./benchmark/files/tmp/tmp2.txt"))
alpha_6 := read(open("./benchmark/files/tmp/tmp3.txt"))
{if 7218306. < 3610599. {start := time.Now()
{write(alpha_1, alpha_4)}
{write(alpha_2, alpha_5)}
{write(alpha_3, alpha_6)}
elapsed := time.Since(start)
print(elapsed.Nanoseconds())} else {start := time.Now()
var wg_alpha_8 sync.WaitGroup
wg_alpha_8.Add(3)
go func(){
{write(alpha_1, alpha_4)}
wg_alpha_8.Done()
}()
go func(){
{write(alpha_2, alpha_5)}
wg_alpha_8.Done()
}()
go func(){
{write(alpha_3, alpha_6)}
wg_alpha_8.Done()
}()
wg_alpha_8.Wait()
elapsed := time.Since(start)
print(elapsed.Nanoseconds())}}
}