package main

import "sync"

var x int
var y bool
var z string
var a int = 1
var b bool = true
var c string = "Hello world"

func main() {
	var wg sync.WaitGroup

	println("Hello world")

	wg.Add(1)
	go func() { println("Hello world"); wg.Done() }()
	wg.Wait()
}
