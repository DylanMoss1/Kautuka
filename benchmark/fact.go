package main 

func fact(x uint64) uint64 {
	total := uint64(1)

	for i := uint64(1); i < x; i++ { 
		total *= i 
	}

	return total
}

func main() {
	print(fact(60))
}