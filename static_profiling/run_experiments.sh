go build -gcflags '-N -l' measure_instruction_runtime.go
./measure_instruction_runtime
rm measure_instruction_runtime