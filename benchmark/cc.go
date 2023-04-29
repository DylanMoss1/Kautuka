package main 

func fact(x uint64) uint64 {
	total := uint64(1)

	for i := uint64(1); i < x; i++ { 
		total *= i 
	}

	return total 
}

func main() {
	for i := uint64(0); i < 10000; i++ { 
		fact(i)
	}
}