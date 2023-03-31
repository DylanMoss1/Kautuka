package main 

func main(){ 
	x := 0

	{
		x = 1
		x := 2

		_ = x 
	}
	{
		x = 1
		x := 2

		_ = x
	}

	_ = x 
}