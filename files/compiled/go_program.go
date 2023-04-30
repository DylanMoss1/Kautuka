package main

import ("fmt"
"os"
"sync")
func check(err error) {
	if err != nil {
			panic(err)
	}
}

func open(filename string) *os.File {
file, err := os.OpenFile(filename, os.O_APPEND|os.O_WRONLY, 0666)
check(err)
return file
}

func read(file *os.File) string {
  dat, err := os.ReadFile(file.Name())
  check(err)
  return string(dat)
}



func main ()  {
alpha_1 := open("./test_files/a/b.txt")
alpha_2 := open("./test_files/a/c.txt")
alpha_3 := alpha_2
alpha_4 := ""
alpha_5 := ""
alpha_6 := ""
{if 906. < 300 {{alpha_4 = read(alpha_1)}
{alpha_5 = read(alpha_2)}
{alpha_6 = read(alpha_3)}} else {var wg_alpha_8 sync.WaitGroup
wg_alpha_8.Add(3)
go func(){
{alpha_4 = read(alpha_1)}
wg_alpha_8.Done()
}()
go func(){
{alpha_5 = read(alpha_2)}
wg_alpha_8.Done()
}()
go func(){
{alpha_6 = read(alpha_3)}
wg_alpha_8.Done()
}()
wg_alpha_8.Wait()}}
fmt.Println(alpha_4)
fmt.Println(alpha_5)
fmt.Println(alpha_6)
}