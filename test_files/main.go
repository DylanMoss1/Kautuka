// package main

// func f(x int) {
// 	for i := 0; i < x; i++ {
// 		print(i)
// 	}
// }

// func main() {
// 	user_input := input()
// 	user_input_len := len(user_input)
// 	if user_input_len > 5 {
// 		f(user_input_len)
// 	} else {
// 		file := open(user_input)
// 		print(read(file))
// 	}
// }

// package main

// func main() {
// 	file, _ := os.OpenFile("a/b.txt", os.O_RDWR, 0777)

// 	print(file.Name())
// }

package main

import (
	"os"
)

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

func write(file *os.File, contents string) {
	file, err := os.Create(file.Name())
	check(err) 
	_, err = file.WriteString(contents)
	check(err)
}

func append(file *os.File, contents string) {
	_, err := file.WriteString(contents)
	check(err)
}

func main() {
	x := open("./test_files/a/b.txt")
	write(x, "hello world")
	print(read(x))
	write(x, "a")
	append(x, "a")
	print(read(x))
}
