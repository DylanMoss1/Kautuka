package main

import (
	"fmt"
	"os"
	"time"
)

func fib(x int) {
	
}

func main() {

	fib(40)
	fib(40)

	start := time.Now()
	fib(40)
	elapsed := time.Since(start)
	print(elapsed.Nanoseconds())

}
