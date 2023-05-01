#!/bin/bash

suppress_output=false
debug=false
benchmark=false

print_usage() {
  printf "Usage: ./kau.sh [-s] [-d] [-b] [-i] [-o <output>] <file>"
}

while getopts 'sdbio:' OPTION; do
  case "$OPTION" in
  o)
    output_file=${OPTARG}
    ;;
  i)
    intermediary_steps=${OPTARG}
    ;;
  s)
    suppress_output=true
    ;;
  d)
    debug=true
    ;;
  b)
    benchmark=true
    ;;
  esac
done
shift "$(($OPTIND - 1))"

input_file=$1

rm -r ./files/intermediary_steps/*

if [ -z $output_file ]; then
  output_file="${input_file%.*}.go"
fi

if [ $debug = true ]; then
  debug_str="--debug"
else
  debug_str=""
fi

rm $output_file >/dev/null 2>&1

execute_benchmark() {
  start=$(date +%s%N)
  end=$(date +%s%N)
  time_delay=$(($end - $start))

  dune exec kautuka -- --seq >/dev/null 2>&1
  start=$(date +%s%N)
  go run $output_file >/dev/null 2>&1
  end=$(date +%s%N)
  
  start=$(date +%s%N)
  go run $output_file >/dev/null 2>&1
  end=$(date +%s%N)
  echo "Parallelised elapsed time: $((($end - $start - $time_delay) / 1000)) us"

  dune exec kautuka -- --seq >/dev/null 2>&1
  start=$(date +%s%N)
  go run $output_file >/dev/null 2>&1
  end=$(date +%s%N)
  echo "Sequential elapsed time: $((($end - $start - $time_delay) / 1000)) us"
}

execute_normal() {
  go run $output_file
}

run_scipt() {
  dune exec kautuka -- $debug_str $input_file $output_file

  if [ $benchmark = true ]; then
    execute_benchmark
  else
    execute_normal
  fi
}

if [ $suppress_output = true ]; then
  run_scipt >/dev/null 2>&1
else
  run_scipt
fi