package main

func main() { 
	x := 0 
	{
		x := 1 
		print(x)
	}
	print(x)
}