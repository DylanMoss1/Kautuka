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
	// Default
	for i := 1; i <= 3; i++ {
		// Default
		fmt.Println(i)
	}
	// Default
	for j := 7; j <= 9; j++ {
		// Default
		fmt.Println(j)
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
