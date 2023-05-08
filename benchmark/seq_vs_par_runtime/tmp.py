import subprocess

programs = ["fact", "factor_sum", "parse_files", "generate_files", "add_footer",
            "letter_count", "print_contents", "count_vowels", "seperate_words", "map_chars"]

if __name__ == "__main__":
  for program in programs:
    command = f"./kau.sh -o ./benchmark/build/{program}.go ./benchmark/corpus/{program}.kau".split(" ")
    subprocess.run(command)