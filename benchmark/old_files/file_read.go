package main

import (
	"os"; "sync"
)

func word_count(file_name string) int {
	file, _ := os.OpenFile(file_name, os.O_RDWR, 0777)
	defer file.Close()
	dat, _ := os.ReadFile(file.Name())
	data := string(dat)
	total := 0
	for _, s := range data {
		if string(s) == " " {
			total += 1
		}
	}
	return total
}

func count_words_in_files(n int) { 

	var x sync.WaitGroup
	x.Add(3)

	a := 0 
	b := 0 
	c := 0 

	go func() {
		for i := 0; i < n; i++ {
			a = word_count("files/a.txt")
		}
		x.Done()
	}()

	go func() {
		for i := 0; i < n; i++ {
			b = word_count("files/b.txt")
		}
		x.Done()
	}()

	go func() {
		for i := 0; i < n; i++ {
			c = word_count("files/c.txt")
		}
		x.Done()
	}()

	x.Wait()

	print(a)
	print(b)
	print(c)

	// for i := 0; i < 100; i++ {

	// 	var x sync.WaitGroup
	// 	x.Add(3)

	// 	go func() {
	// 		print(read_file("files/a.txt"))
	// 		x.Done()
	// 	}()

	// 	go func() {
	// 		print(read_file("files/b.txt"))
	// 		x.Done()

	// 	}()

	// 	go func() {
	// 		print(read_file("files/c.txt"))
	// 		x.Done()

	// 	}()

	// 	x.Wait()
	// }

	// for i := 0; i < 1000; i++ {
	// 	print(read_file("files/a.txt"))
	// 	print(read_file("files/b.txt"))
	// 	print(read_file("files/c.txt"))
	// }
}

func main() {
	count_words_in_files(500) 
}
