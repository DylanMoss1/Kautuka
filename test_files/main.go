package main 

var x = 5 

func f2() { 
	f1()
}

func f1() { 
	x = x + 5
}

func main() { 
	for i := 0; i < 10; i++  {
		f2() 
		println(x)
	}
}