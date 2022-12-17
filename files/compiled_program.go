package main

import (
	"fmt"
)

var c string = "Hello world"
var b bool = true
var a int = 1
var z string
var y bool
var x int

func main() {
	// Default
	// Default
	for n := 0; n <= 5; n++ {
		// Default
		fmt.Println(n)
		if n%2 == 0 {
			// Default
			continue
		}
	}
	for true {
		// Default
		break
		fmt.Println("loop")
	}
	// Default
	for j := 7; j <= 9; j++ {
		// Default
		fmt.Println(j)
	}
	for i <= 3 {
		// Default
		i = i + 1
		fmt.Println(i)
	}
	i := 1
}
