package main

import (
	"fmt"
	"sync"
)

func main() {
	x := 0
	{
		var wg_alpha_6 sync.WaitGroup
		wg_alpha_6.Add(2)
		go func() {
			{
				y := 1
				x = 5
				_ = y
			}
			wg_alpha_6.Done()
		}()
		go func() {
			{
				z := 0
				_ = z
			}
			wg_alpha_6.Done()
		}()
		wg_alpha_6.Wait()
	}
	fmt.Println(x)
}
