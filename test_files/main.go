package main

import (
	"sync"
)

func main() {

	var wg sync.WaitGroup

	wg.Add(1)
	go func() {
		{
			print(1)
			print(2)
		}
		{
			print(3)
			print(4)
		}

		wg.Done()
	}()
	wg.Wait()

	{
		print(5)
		print(6)
	}
}
