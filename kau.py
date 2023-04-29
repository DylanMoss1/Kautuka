import sys 
import os 

def remove_debug_files(): 
    os.system("rm -r ./files/intermediary_steps >/dev/null 2>&1")
    os.system("mkdir ./files/intermediary_steps")
    os.system("rm ./files/go_program.go >/dev/null 2>&1")

if __name__ == "__main__": 

    if len(sys.argv) != 4: 
        print("Error: incorrect number of args")
        exit(1)

    run_go = sys.argv[1]
    debug = sys.argv[2]
    benchmark = sys.argv[3]
    
    print("run go:", run_go)
    print("debug:", debug)
    print("benchmark:", benchmark)

    if debug:
        remove_debug_files() 

    os.system("dune build")

    if debug:
        os.system("dune exec kautuka --debug")
    else: 
        os.system("dune exec kautuka")



    os.system("gofmt -w ./files/compiled_program.go")
    os.system("go run ./files/compiled_program.go")

    # dune build 
# dune exec kautuka --debug