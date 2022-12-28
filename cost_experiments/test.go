package main

import (
	"fmt"
	"os"
	"time"

	"github.com/montanaflynn/stats"
)

func dud_operation() { 
	for i := 0; i < 100; i++ { 
		y := 1
		z := y + 5 
		_, _ = y, z 
	}
}

func empty() int64 { 
	start := time.Now()
	elapsed := time.Since(start)
	return elapsed.Nanoseconds()
}

func declInt() int64 {
	start := time.Now()

	x := 100000000000000

	elapsed := time.Since(start)

	_ = x

	return elapsed.Nanoseconds()
}

func openFile() int64 {
	start := time.Now()

	f, err := os.Open("test_files/empty_file.txt")

	elapsed := time.Since(start)

	_, _ = f, err

	return elapsed.Nanoseconds()
}

func printResults(category string, results []float64) {

	fmt.Printf("%s:\n", category)

	lowest, _ := stats.Min(results)
	highest, _ := stats.Max(results)
	quartiles, _ := stats.Quartile(results)
	percentile_10, _ := stats.Percentile(results, 10)
	percentile_90, _ := stats.Percentile(results, 90)

	lower_quartile := quartiles.Q1
	median := quartiles.Q2
	upper_quartile := quartiles.Q3

	fmt.Println("  - Lowest:", lowest)
	fmt.Println("  - P10th:", percentile_10)
	fmt.Println("  - Q1:", lower_quartile)
	fmt.Println("  - Median:", median)
	fmt.Println("  - Q3:", upper_quartile)
	fmt.Println("  - P90th:", percentile_90)
	fmt.Println("  - Highest:", highest)

	fmt.Println("")
}

func runExperiment(category string, f func() int64) {
	results := []float64{}

	for i := 0; i < 100; i++ {
		results = append(results, float64(f()))
	}

	printResults(category, results)
}

func main() {
	runExperiment("empty", empty)
	runExperiment("declInt", declInt)
}
