package main

import (
	"fmt"
	"os"
	"time"
)

func openFile() int64 {
	start := time.Now()

	f, err := os.Open("test_files/empty_file.txt")

	elapsed := time.Since(start)

	_, _ = f, err

	time.Sleep(2 * time.Second)

	return elapsed.Nanoseconds()
}

func avg(xs []int64) int64 { 
	sum := int64(0) 

	for i := 0; i < len(xs); i++ { 
		sum += xs[i]
	}

	return sum / int64(len(xs))
}

func main() {
	results := []int64{}

	for i := 0; i < 10; i++ { 
		results = append(results, openFile())
	}

	fmt.Println(results)
	fmt.Println(avg(results))
}
