while getopts 'o:' OPTION; do
  case "$OPTION" in
  o)
    output_file=${OPTARG}
    ;;
  esac
done
shift "$(($OPTIND - 1))"

input_file=$1

if [ -z $output_file ]; then
  output_file="${input_file%.*}.go"
fi

dune exec -- ./src/bin/main.exe $input_file $output_file
go build ./benchmark/build/x.go