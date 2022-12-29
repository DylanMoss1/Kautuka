package main

import (
	"fmt"
)

var x int
var y bool
var z string
var a int = 1
var b bool = true
var c string = "Hello world"

func main() {
	// Default
	i := 1
	for i <= 3 {
		// Default
		fmt.Println(i)
		i = i + 1
	}
	// Default
	for j := 7; j <= 9; j++ {
		// Default
		fmt.Println(j)
	}
	for true {
		// Default
		fmt.Println("loop")
		break
	}
	// Default
	for n := 0; n <= 5; n++ {
		// Default
		if n == 0 {
			// Default
			continue
		}
		fmt.Println(n)
	}
}
