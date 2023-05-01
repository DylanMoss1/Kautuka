//usr/bin/go run $0 $@ ; exit

package main

import (
	"encoding/csv"
	"fmt"
	"math"
	"os"
	"strings"
	"sync"
	"time"
)

func input() string {
	var __s string
	fmt.Scan(&__s)
	return __s
}

func check(err error) {
	if err != nil {
		panic(err)
	}
}

func open(filename string) *os.File {
	file, err := os.OpenFile(filename, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0666)
	check(err)
	return file
}

func read(file *os.File) string {
	dat, err := os.ReadFile(file.Name())
	check(err)
	return string(dat)
}

func write(file *os.File, contents string) {
	file, err := os.Create(file.Name())
	check(err)
	_, err = file.WriteString(contents)
	check(err)
}

func append(file *os.File, contents string) {
	_, err := file.WriteString(contents)
	check(err)
}

func dud_operation() int {

	total := 0

	for i := 0; i < 100; i++ {
		y := i
		total += y + 5
		if y > 200 {
			break
		}
	}

	return total
}

func get_empty_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()
	elapsed := time.Since(start)

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_assign_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()
	x := 5
	elapsed := time.Since(start)

	_ = x

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_var_read_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()
	x := i
	elapsed := time.Since(start)

	_ = x

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_add_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()
	x := 112415814235273431 + i*8194235273431
	elapsed := time.Since(start)

	_ = x

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_mult_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()
	x := 1124143431 * i
	elapsed := time.Since(start)

	_ = x

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_bool_cond_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()
	x := i < 1000
	elapsed := time.Since(start)

	_ = x

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_concat_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()
	x := "afjsigrouetgosrafjsigrouetgosreajroigoeajroafjsigrouetgosreajroigoigo" + "afjsigrouetgosreajroigoafjsigrouetgosreajroigoafjsigrouetgosreajroigo"
	elapsed := time.Since(start)

	_ = x

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_increment_runtime_cost(i int) int64 {
	_ = dud_operation()

	x := i
	start := time.Now()
	x++
	elapsed := time.Since(start)

	_ = x

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_if_statement_runtime_cost(i int) int64 {
	_ = dud_operation()

	total := 0

	start := time.Now()
	if i < 1000 {
		total++
	} else {
		total--
	}
	elapsed := time.Since(start)

	_ = total

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_for_loop_runtime_cost(i int) int64 {
	_ = dud_operation()

	start1 := time.Now()

	j := 0
	j++
	t := j < 10
	j++
	t = j < 10
	j++
	t = j < 10
	j++
	t = j < 10
	j++
	t = j < 10
	j++
	t = j < 10
	j++
	t = j < 10
	j++
	t = j < 10
	j++
	t = j < 10
	j++
	t = j < 10

	total1 := 0
	total1 += 0
	total1 += 1
	total1 += 2
	total1 += 3
	total1 += 4
	total1 += 5
	total1 += 6
	total1 += 7
	total1 += 8
	total1 += 9

	elapsed1 := time.Since(start1)

	_ = total1
	_ = t

	total2 := 0

	start2 := time.Now()

	for i := 0; i < 10; i++ {
		total2 += i
	}

	elapsed2 := time.Since(start2)

	_ = total2

	if i > 2 {
		return elapsed2.Nanoseconds() - elapsed1.Nanoseconds()
	} else {
		return 0
	}
}

func get_for_each0_runtime_cost(i int) int64 {
	_ = dud_operation()

	str := strings.Repeat("h", 0)

	item1 := ""
		
	start1 := time.Now()

	elapsed1 := time.Since(start1)

	_ = item1

	item2 := ""

	start2 := time.Now()

	for _, s := range str {
		item2 += string(s)
	}

	elapsed2 := time.Since(start2)

	_ = str
	_ = item2

	if i > 2 {
		return elapsed2.Nanoseconds() - elapsed1.Nanoseconds()
	} else {
		return 0
	}
}

func get_for_each1_runtime_cost(i int) int64 {
	_ = dud_operation()

	str := strings.Repeat("h", 1)

	item1 := ""
	
	start1 := time.Now()

	item1 += string(str[0])

	elapsed1 := time.Since(start1)

	_ = item1

	item2 := ""

	start2 := time.Now()

	for _, s := range str {
		item2 += string(s)
	}

	elapsed2 := time.Since(start2)

	_ = str
	_ = item2

	if i > 2 {
		return elapsed2.Nanoseconds() - elapsed1.Nanoseconds()
	} else {
		return 0
	}
}

func get_for_each2_runtime_cost(i int) int64 {
	_ = dud_operation()

	str := strings.Repeat("h", 2)

	item1 := ""

	start1 := time.Now()

	item1 += string(str[0])
	item1 += string(str[1])

	elapsed1 := time.Since(start1)

	_ = item1

	item2 := ""

	start2 := time.Now()

	for _, s := range str {
		item2 += string(s)
	}

	elapsed2 := time.Since(start2)

	_ = str
	_ = item2

	if i > 2 {
		return elapsed2.Nanoseconds() - elapsed1.Nanoseconds()
	} else {
		return 0
	}
}
func get_for_each3_runtime_cost(i int) int64 {
	_ = dud_operation()

	str := strings.Repeat("h", 3)

	item1 := ""

	start1 := time.Now()

	item1 += string(str[0])
	item1 += string(str[1])
	item1 += string(str[2])

	elapsed1 := time.Since(start1)

	_ = item1

	item2 := ""

	start2 := time.Now()

	for _, s := range str {
		item2 += string(s)
	}

	elapsed2 := time.Since(start2)

	_ = str
	_ = item2

	if i > 2 {
		return elapsed2.Nanoseconds() - elapsed1.Nanoseconds()
	} else {
		return 0
	}
}
func get_for_each4_runtime_cost(i int) int64 {
	_ = dud_operation()

	str := strings.Repeat("h", 4)

	item1 := ""

	start1 := time.Now()

	item1 += string(str[0])
	item1 += string(str[1])
	item1 += string(str[2])
	item1 += string(str[3])

	elapsed1 := time.Since(start1)

	_ = item1

	item2 := ""

	start2 := time.Now()

	for _, s := range str {
		item2 += string(s)
	}

	elapsed2 := time.Since(start2)

	_ = str
	_ = item2

	if i > 2 {
		return elapsed2.Nanoseconds() - elapsed1.Nanoseconds()
	} else {
		return 0
	}
}
func get_for_each5_runtime_cost(i int) int64 {
	_ = dud_operation()

	str := strings.Repeat("h", 5)

	item1 := ""

	start1 := time.Now()

	item1 += string(str[0])
	item1 += string(str[1])
	item1 += string(str[2])
	item1 += string(str[3])
	item1 += string(str[4])

	elapsed1 := time.Since(start1)

	_ = item1

	item2 := ""

	start2 := time.Now()

	for _, s := range str {
		item2 += string(s)
	}

	elapsed2 := time.Since(start2)

	_ = str
	_ = item2

	if i > 2 {
		return elapsed2.Nanoseconds() - elapsed1.Nanoseconds()
	} else {
		return 0
	}
}
func get_for_each6_runtime_cost(i int) int64 {
	_ = dud_operation()

	str := strings.Repeat("h", 6)

	item1 := ""

	start1 := time.Now()

	item1 += string(str[0])
	item1 += string(str[1])
	item1 += string(str[2])
	item1 += string(str[3])
	item1 += string(str[4])
	item1 += string(str[5])

	elapsed1 := time.Since(start1)

	_ = item1

	item2 := ""

	start2 := time.Now()

	for _, s := range str {
		item2 += string(s)
	}

	elapsed2 := time.Since(start2)

	_ = str
	_ = item2

	if i > 2 {
		return elapsed2.Nanoseconds() - elapsed1.Nanoseconds()
	} else {
		return 0
	}
}
func get_for_each7_runtime_cost(i int) int64 {
	_ = dud_operation()

	str := strings.Repeat("h", 7)

	item1 := ""

	start1 := time.Now()

	item1 += string(str[0])
	item1 += string(str[1])
	item1 += string(str[2])
	item1 += string(str[3])
	item1 += string(str[4])
	item1 += string(str[5])
	item1 += string(str[6])

	elapsed1 := time.Since(start1)

	_ = item1

	item2 := ""

	start2 := time.Now()

	for _, s := range str {
		item2 += string(s)
	}

	elapsed2 := time.Since(start2)

	_ = str
	_ = item2

	if i > 2 {
		return elapsed2.Nanoseconds() - elapsed1.Nanoseconds()
	} else {
		return 0
	}
}

func get_for_each8_runtime_cost(i int) int64 {
	_ = dud_operation()

	str := strings.Repeat("h", 8)

	item1 := ""

	start1 := time.Now()

	item1 += string(str[0])
	item1 += string(str[1])
	item1 += string(str[2])
	item1 += string(str[3])
	item1 += string(str[4])
	item1 += string(str[5])
	item1 += string(str[6])
	item1 += string(str[7])

	elapsed1 := time.Since(start1)

	_ = item1

	item2 := ""

	start2 := time.Now()

	for _, s := range str {
		item2 += string(s)
	}

	elapsed2 := time.Since(start2)

	_ = str
	_ = item2

	if i > 2 {
		return elapsed2.Nanoseconds() - elapsed1.Nanoseconds()
	} else {
		return 0
	}
}

func get_for_each9_runtime_cost(i int) int64 {
	_ = dud_operation()

	str := strings.Repeat("h", 9)

	item1 := ""

	start1 := time.Now()

	item1 += string(str[0])
	item1 += string(str[1])
	item1 += string(str[2])
	item1 += string(str[3])
	item1 += string(str[4])
	item1 += string(str[5])
	item1 += string(str[6])
	item1 += string(str[7])
	item1 += string(str[8])

	elapsed1 := time.Since(start1)

	_ = item1

	item2 := ""

	start2 := time.Now()

	for _, s := range str {
		item2 += string(s)
	}

	elapsed2 := time.Since(start2)

	_ = str
	_ = item2

	if i > 2 {
		return elapsed2.Nanoseconds() - elapsed1.Nanoseconds()
	} else {
		return 0
	}
}

func get_for_each10_runtime_cost(i int) int64 {
	_ = dud_operation()

	str := strings.Repeat("h", 10)

	item1 := ""

	start1 := time.Now()

	item1 += string(str[0])
	item1 += string(str[1])
	item1 += string(str[2])
	item1 += string(str[3])
	item1 += string(str[4])
	item1 += string(str[5])
	item1 += string(str[6])
	item1 += string(str[7])
	item1 += string(str[8])
	item1 += string(str[9])

	elapsed1 := time.Since(start1)

	_ = item1

	item2 := ""

	start2 := time.Now()

	for _, s := range str {
		item2 += string(s)
	}

	elapsed2 := time.Since(start2)

	_ = str
	_ = item2

	if i > 2 {
		return elapsed2.Nanoseconds() - elapsed1.Nanoseconds()
	} else {
		return 0
	}
}

func get_print_runtime_cost(i int, size int) int64 {
	_ = dud_operation()

	str := strings.Repeat("h", size)

	temp := os.Stdout
	os.Stdout = nil

	start := time.Now()
	fmt.Println(str)
	elapsed := time.Since(start)

	os.Stdout = temp

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_input_runtime_cost() float64 {
	start := time.Now()
	_ = input()
	elapsed := time.Since(start)

	print(elapsed.Nanoseconds())

	return float64(elapsed.Nanoseconds())
}

func get_open_runtime_cost(i int) int64 {
	_ = dud_operation()

	filename := "./test_files/test_file.txt"

	start := time.Now()
	file := open(filename)
	elapsed := time.Since(start)

	_ = file

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_read_runtime_cost(i int, size int) int64 {
	_ = dud_operation()

	filename := "./test_files/test_file.txt"
	file := open(filename)

	contents := strings.Repeat("h", size)
	write(file, contents)

	start := time.Now()
	str := read(file)
	elapsed := time.Since(start)

	_ = file
	_ = str

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_write_runtime_cost(i int, size int) int64 {
	_ = dud_operation()

	filename := "./test_files/test_file.txt"
	file := open(filename)
	str := strings.Repeat("h", size)

	start := time.Now()
	write(file, str)
	elapsed := time.Since(start)

	_ = file
	_ = str

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_append_runtime_cost(i int, size int) int64 {
	_ = dud_operation()

	filename := "./test_files/test_file.txt"
	file := open(filename)
	write(file, "")
	str := strings.Repeat("h", size)

	start := time.Now()
	append(file, str)
	elapsed := time.Since(start)

	_ = file
	_ = str

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func identity(i int) int {
	return i
}

func get_func_call_runtime_cost(i int) int64 {
	_ = dud_operation()

	total := 0

	start := time.Now()
	total += identity(i)
	elapsed := time.Since(start)

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_parallelisation2_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()

	var wg sync.WaitGroup

	wg.Add(2)

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	wg.Wait()

	elapsed := time.Since(start)

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_parallelisation3_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()

	var wg sync.WaitGroup

	wg.Add(3)

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	wg.Wait()

	elapsed := time.Since(start)

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_parallelisation4_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()

	var wg sync.WaitGroup

	wg.Add(4)

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	wg.Wait()

	elapsed := time.Since(start)

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_parallelisation5_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()

	var wg sync.WaitGroup

	wg.Add(5)

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	wg.Wait()

	elapsed := time.Since(start)

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_parallelisation6_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()

	var wg sync.WaitGroup

	wg.Add(6)

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()
	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	wg.Wait()

	elapsed := time.Since(start)

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_parallelisation7_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()

	var wg sync.WaitGroup

	wg.Add(7)

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()
	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	wg.Wait()

	elapsed := time.Since(start)

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_parallelisation8_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()

	var wg sync.WaitGroup

	wg.Add(8)

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()
	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	wg.Wait()

	elapsed := time.Since(start)

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_parallelisation9_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()

	var wg sync.WaitGroup

	wg.Add(9)

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	wg.Wait()

	elapsed := time.Since(start)

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func get_parallelisation10_runtime_cost(i int) int64 {
	_ = dud_operation()

	start := time.Now()

	var wg sync.WaitGroup

	wg.Add(10)

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	go func() {
		wg.Done()
	}()

	wg.Wait()

	elapsed := time.Since(start)

	if i > 2 {
		return elapsed.Nanoseconds()
	} else {
		return 0
	}
}

func run_experiment(experiment func(int) int64, experiment_name string, subtraction float64) float64 {
	total := int64(0)
	epochs := 1000000

	for i := 0; i < epochs+3; i++ {
		total += experiment(i)
	}

	time_taken := math.Max((float64(total)/float64(epochs))-subtraction, 0.)

	trace_notation := ""

	if time_taken < 1. {
		trace_notation = " (tr)"
	}

	print(fmt.Sprintf("%s: %f%s\n", experiment_name, time_taken, trace_notation))

	return time_taken
}

func make_seq(len int) []int {
	seq := make([]int, len)
	for i := range seq {
		seq[i] = int(math.Pow(float64(i), 3.))
	}
	return seq
}

func run_experiment_scaled(experiment func(int, int) int64, experiment_name string, num_sizes int, subtraction_init float64, subtraction_iter float64) []float64 {

	size_seq := make_seq(num_sizes)
	runtime_seq := make([]float64, num_sizes)

	for index, size := range size_seq {
		total := int64(0)
		epochs := 1000

		for i := 0; i < epochs+3; i++ {
			total += experiment(i, size)
		}

		time_taken := math.Max((float64(total)/float64(epochs))-subtraction_init-subtraction_iter*float64(size), 0.)
		runtime_seq[index] = time_taken
	}

	return runtime_seq
}

func main() {
	filename := "./results/results.csv"

	csv_file := open(filename)
	write(csv_file, "")
	csv_writer := csv.NewWriter(csv_file)
	csv_writer.Write([]string{"print", "read", "write", "append"})

	csv_writer.Flush()

	empty := run_experiment(get_empty_runtime_cost, "empty", 0.)
	assign := run_experiment(get_assign_runtime_cost, "assign", empty)
	var_read := run_experiment(get_var_read_runtime_cost, "var read", empty+assign)
	add := run_experiment(get_add_runtime_cost, "add", empty+assign)
	run_experiment(get_mult_runtime_cost, "mult", empty+assign)
	run_experiment(get_concat_runtime_cost, "concat", empty+assign)
	bool_cond := run_experiment(get_bool_cond_runtime_cost, "bool cond", empty+assign)
	increment := run_experiment(get_increment_runtime_cost, "increment", empty)
	run_experiment(get_if_statement_runtime_cost, "if statement", empty+increment+bool_cond)
	run_experiment(get_open_runtime_cost, "open", empty+assign+var_read)
	run_experiment(get_func_call_runtime_cost, "func call", empty+var_read+add+assign)
	run_experiment(get_for_loop_runtime_cost, "for loop", 0.)

	for_each0 := run_experiment(get_for_each0_runtime_cost, "for each 0", 0.)
	for_each1 := run_experiment(get_for_each1_runtime_cost, "for each 1", 0.)
	for_each2 := run_experiment(get_for_each2_runtime_cost, "for each 2", 0.)
	for_each3 := run_experiment(get_for_each3_runtime_cost, "for each 3", 0.)
	for_each4 := run_experiment(get_for_each4_runtime_cost, "for each 4", 0.)
	for_each5 := run_experiment(get_for_each5_runtime_cost, "for each 5", 0.)
	for_each6 := run_experiment(get_for_each6_runtime_cost, "for each 6", 0.)
	for_each7 := run_experiment(get_for_each7_runtime_cost, "for each 7", 0.)
	for_each8 := run_experiment(get_for_each8_runtime_cost, "for each 8", 0.)
	for_each9 := run_experiment(get_for_each9_runtime_cost, "for each 9", 0.)
	for_each10 := run_experiment(get_for_each10_runtime_cost, "for each 10", 0.)

	csv_writer.Write([]string{fmt.Sprintf("%f", for_each0), fmt.Sprintf("%f", for_each1), fmt.Sprintf("%f", for_each2), fmt.Sprintf("%f", for_each3), fmt.Sprintf("%f", for_each4), fmt.Sprintf("%f", for_each5), fmt.Sprintf("%f", for_each6), fmt.Sprintf("%f", for_each7), fmt.Sprintf("%f", for_each8), fmt.Sprintf("%f", for_each9), fmt.Sprintf("%f", for_each10)})

	par2 := run_experiment(get_parallelisation2_runtime_cost, "parallelisation 2", empty)
	par3 := run_experiment(get_parallelisation3_runtime_cost, "parallelisation 3", empty)
	par4 := run_experiment(get_parallelisation4_runtime_cost, "parallelisation 4", empty)
	par5 := run_experiment(get_parallelisation5_runtime_cost, "parallelisation 5", empty)
	par6 := run_experiment(get_parallelisation6_runtime_cost, "parallelisation 6", empty)
	par7 := run_experiment(get_parallelisation7_runtime_cost, "parallelisation 7", empty)
	par8 := run_experiment(get_parallelisation8_runtime_cost, "parallelisation 8", empty)
	par9 := run_experiment(get_parallelisation9_runtime_cost, "parallelisation 9", empty)
	par10 := run_experiment(get_parallelisation10_runtime_cost, "parallelisation 10", empty)

	csv_writer.Write([]string{fmt.Sprintf("%f", par2), fmt.Sprintf("%f", par3), fmt.Sprintf("%f", par4), fmt.Sprintf("%f", par5), fmt.Sprintf("%f", par6), fmt.Sprintf("%f", par7), fmt.Sprintf("%f", par8), fmt.Sprintf("%f", par9), fmt.Sprintf("%f", par10)})

	num_sizes := 70

	fmt.Println("get print...")

	print := run_experiment_scaled(get_print_runtime_cost, "print", num_sizes, empty, 0.)

	fmt.Println("get read...")

	read := run_experiment_scaled(get_read_runtime_cost, "read", num_sizes, empty+assign+var_read, 0.)

	fmt.Println("get write...")

	_write := run_experiment_scaled(get_write_runtime_cost, "write", num_sizes, empty+2*var_read, 0.)

	fmt.Println("get append...")

	append := run_experiment_scaled(get_append_runtime_cost, "append", num_sizes, empty+2*var_read, 0.)

	for i := 0; i < num_sizes; i++ {
		csv_writer.Write([]string{fmt.Sprintf("%f", print[i]), fmt.Sprintf("%f", read[i]), fmt.Sprintf("%f", _write[i]), fmt.Sprintf("%f", append[i])})
	}

	filename = "./test_files/test_file.txt"
	file := open(filename)
	write(file, "")

	csv_writer.Flush()
	csv_file.Close()
}
