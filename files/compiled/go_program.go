package main

import (
	"fmt"
	"sync"
)

func main() {
	alpha_1 := 0
	{
		var wg_alpha_4 sync.WaitGroup
		wg_alpha_4.Add(2)
		go func() {
			{
				{
					for alpha_2 := 0; alpha_2 < 500; alpha_2++ {
						fmt.Println("hello world")
					}
				}
			}
			wg_alpha_4.Done()
		}()
		go func() {
			{
				{
					for alpha_3 := 0; alpha_3 < 500; alpha_3++ {
						alpha_1 = alpha_1 + alpha_3
					}
				}
			}
			wg_alpha_4.Done()
		}()
		wg_alpha_4.Wait()
	}
	fmt.Println(alpha_1)
}
