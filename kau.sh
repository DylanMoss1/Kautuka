#!/bin/bash 

rm -r ./files/intermediary_steps > /dev/null 2>&1
mkdir ./files/intermediary_steps 
rm ./files/go_program.go > /dev/null 2>&1

dune build
dune exec kautuka $@ 
# gofmt -w ./files/compiled_program.go
# go run ./files/compiled_program.go 