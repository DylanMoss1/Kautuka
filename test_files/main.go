package main

func main() {
	y := "hello world"
	for _, x := range y {
		var x string = string(x)
		print(x)
	}
}
