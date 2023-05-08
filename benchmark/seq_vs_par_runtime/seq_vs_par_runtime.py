import subprocess

programs = ["fact", "factor_sum", "parse_files", "generate_files", "add_footer",
            "letter_count", "print_contents", "count_vowels", "seperate_words", "map_chars"]

if __name__ == "__main__":
  with open("./benchmark/seq_vs_par_runtime/results/results.txt", "w") as outfile:
    outfile.write("")
  
  with open("./benchmark/seq_vs_par_runtime/results/results.txt", "a") as outfile:
    for i in range(10): 
      for flag in ["", "-s "]:
        for program in programs:
          command = f"./kau.sh {flag}-o ./benchmark/build/{program}.go ./benchmark/corpus/{program}.kau".split(" ")
          subprocess.run(command, stderr=outfile)
          outfile.write("\n")
          outfile.flush() 
