#!/bin/bash 

rm -r ./files/intermediary_steps
mkdir ./files/intermediary_steps
rm ./files/go_program.go 

dune build
dune exec kautuka $@ 
# gofmt -w ./files/compiled_program.go
# go run ./files/compiled_program.go 