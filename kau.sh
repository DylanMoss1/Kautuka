#!/bin/bash

suppress_output=false
debug=false
benchmark=false

while getopts 'sdb' OPTION; do
  case "$OPTION" in
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

compiled_go="./files/compiled/go_program.go"
compiled_seq_go="./files/compiled/seq_go_program.go"

rm -r ./files/intermediary_steps >/dev/null 2>&1
mkdir ./files/intermediary_steps
rm $compiled_go >/dev/null 2>&1
rm $compiled_seq_go >/dev/null 2>&1

dune build

if $debug; then
  dune exec kautuka -- --debug
else
  dune exec kautuka >/dev/null 2>&1
fi

gofmt -w $compiled_go

if test -f $compiled_go; then
  if $suppress_output; then
    if $benchmark; then
      start=$(date +%s%N)
      end=$(date +%s%N)
      time_delay=$(($end - $start))

      start=$(date +%s%N)
      go run $compiled_go >/dev/null 2>&1
      end=$(date +%s%N)
      echo "Parallelised elapsed time: $((($end - $start - $time_delay) / 1000)) us"

      dune exec kautuka -- --seq >/dev/null 2>&1
      start=$(date +%s%N)
      go run $compiled_seq_go >/dev/null 2>&1
      end=$(date +%s%N)
      echo "Sequential elapsed time: $((($end - $start - $time_delay) / 1000)) us"
    else
      go run $compiled_go >/dev/null 2>&1
    fi
  else
    if $benchmark; then

      start=$(date +%s%N)
      end=$(date +%s%N)
      time_delay=$(($end - $start))

      start=$(date +%s%N)
      go run $compiled_go
      end=$(date +%s%N)
      echo "Parallelised elapsed time: $((($end - $start - $time_delay) / 1000)) us"

      dune exec kautuka -- --seq >/dev/null 2>&1
      gofmt -w $compiled_seq_go
      start=$(date +%s%N)
      go run $compiled_seq_go
      end=$(date +%s%N)
      echo "Sequential elapsed time: $((($end - $start - $time_delay) / 1000)) us"
    else
      go run $compiled_go
    fi
  fi
else
  echo "Error: file $compiled_go does not exist"
fi
