package main

var x int
var y bool
var z string
var a int = 1
var b bool = true
var c string = "Hello world"

func main() {
	i := 1
	for i <= 3 {
		println(i)
		i = i + 1
	}
	for j := 7; j <= 9; j++ {
		println(j)
	}
	for true {
		println("loop")
		break
	}
	for n := 0; n <= 5; n++ {
		if n%2 == 0 {
			continue
		}
		println(n)
	}
}
