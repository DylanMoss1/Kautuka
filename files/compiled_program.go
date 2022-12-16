package main

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
		println(i)
		i = i + 1
	}
	// Default
	for j := 7; j <= 9; j++ {
		// Default
		println(j)
	}
	for true {
		// Default
		println("loop")
		break
	}
	// Default
	for n := 0; n <= 5; n++ {
		// Default
		if n%2 == 0 {
			// Default
			continue
		}
		println(n)
	}
}
