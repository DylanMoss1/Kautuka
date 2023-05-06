#!/bin/bash

suppress_output=false
debug=false
benchmark=false
seq=false 

print_usage() {
  printf "Usage: ./kau.sh [-s] [-d] [-b] [-i] [--seq] [-o <output>] <file>"
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
    seq=true
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

rm ./files/intermediary_steps/* >/dev/null 2>&1

if [ -z $output_file ]; then
  output_file="${input_file%.*}.go"
fi

if [ $debug = true ]; then
  debug_str="--debug"
else
  debug_str=""
fi

if [ $seq = true ]; then
  seq_str="--seq"
else
  seq_str=""
fi


rm $output_file >/dev/null 2>&1
touch $output_file

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
  if [ $debug = false ]; then
    dune exec kautuka -- $debug_str $seq_str $input_file $output_file  >/dev/null 2>&1
  else 
    dune exec kautuka -- $debug_str $seq_str $input_file $output_file
  fi 

  if [ $benchmark = true ]; then
    execute_benchmark
  else
    execute_normal
  fi
}

cp ./benchmark/files/small1.txt ./benchmark/files/tmp/small1.txt
cp ./benchmark/files/small2.txt ./benchmark/files/tmp/small2.txt
cp ./benchmark/files/small2.txt ./benchmark/files/tmp/small2.txt

run_scipt

rm ./benchmark/files/tmp/* >/dev/null 2>&1