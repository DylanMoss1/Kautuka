package main

import (
	"encoding/csv"
	"log"
	"os"
	"strconv"
	"time"
)

func dud_operation() {
	for i := 0; i < 100; i++ {
		y := i
		z := y + 5
		if y+z > 200 {
			break
		}
	}
}

func get_runtime_cost() []string {

	runtime_costs := []string{}
	dud_operation()

	// Empty instruction

	start := time.Now()
	elapsed := time.Since(start)
	runtime_costs = append(runtime_costs, strconv.FormatInt(elapsed.Nanoseconds(), 10))
	dud_operation()

	// Unop: Minus 

	// Binop: Plus

	// Binop: Minus

	start = time.Now()

	_ = 57839284 * 3947319304

	elapsed = time.Since(start)
	runtime_costs = append(runtime_costs, strconv.FormatInt(elapsed.Nanoseconds(), 10))
	dud_operation()

	// Binop: Mult

	// Func call: User func (init)

	// Func call: Print 

	// Func call: Input 

	// Func call: Open 

	// Func call: Read 

	// Func call: Write 

	// Func call: Append 

	// Expr: Var read 

	// Var statement: Var non init

	// Var statement: Var init

	// Var statement: Var decl 

	// Var statement: Var assign 

	// Var statement: Pre inc 

	// Var statement: Pre dec

	// Var statement: Post inc

	// Var statement: Post dec

	// For loop (instantiation)
	
	// For each (instantiation)

	// If statement (instantiation)

	return runtime_costs
}

func main() {
	instruction_names := []string{"Empty instruction", "Unop: Not", "Unop: Minus", "Binop: Plus", "Binop: Minus", "Binop: Mult", "Binop: Lt", "Binop: Le", "Binop: Gt", "Binop: Ge", "Binop: Eq", "Binop: Ne", "Binop: And", "Binop: Or", "Func call: User func (init)", "Func call: Print", "Func call: Input", "Func call: Open", "Func call: Read", "Func call: Write", "Func call: Append", "Expr: Var read", "Var statement: Var non init", "Var statement: Var init", "Var statement: Var decl", "Var statement: Var assign", "Var statement: Pre inc", "Var statement: Pre dec", "Var statement: Post inc", "Var statement: Post dec", "For loop (instantiation)", "For each (instantiation)", "If statement (instantiation)"}
	csv_file, err := os.Create("./results/results.csv")

	if err != nil {
		log.Fatalf("Failed to create file \"./results/results.csv\"")
	}

	csv_writer := csv.NewWriter(csv_file)

	csv_writer.Write(instruction_names)

	for i := 0; i < 20; i++ {
		runtime_costs := get_runtime_cost()

		if i >= 2 {
			_ = csv_writer.Write(runtime_costs)
		}
	}

	csv_writer.Flush()
	csv_file.Close()
}