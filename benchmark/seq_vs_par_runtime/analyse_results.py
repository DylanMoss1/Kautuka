import numpy as np 

programs = ["fact", "factor_sum", "parse_files", "generate_files", "add_footer",
            "letter_count", "print_contents", "count_vowels", "seperate_words", "map_chars"]

if __name__ == "__main__":
  with open("./benchmark/seq_vs_par_runtime/results/results.txt", "r") as outfile:
    lines = outfile.readlines()
    line_no = len(lines)
    iters = int(line_no / 20)

    lines = np.array([int(item) for item in lines])
    lines = lines.reshape(iters, 20)

    total = np.zeros(20)

    for line in lines:
      total += line 

    total /= iters

    for i, program in enumerate(programs):
      for j in range(2):
        if j == 0: 
          flag = "(par)"
        else: 
          flag = "(seq)"
        print(f"{program} {flag}: {total[i + 10 * j]}")
      print()


