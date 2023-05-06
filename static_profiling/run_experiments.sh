#!/bin/bash 

for i in {1..100}
do
  go build -gcflags '-N -l' measure_instruction_runtime2.go
  ./measure_instruction_runtime2
  rm measure_instruction_runtime2
  sleep 0.1
done 