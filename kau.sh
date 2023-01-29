#!/bin/bash 

dune build
dune exec kautuka $@ 
# gofmt -w ./files/compiled_program.go
# go run ./files/compiled_program.go 