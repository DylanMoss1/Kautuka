import csv
import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression

model = LinearRegression(fit_intercept=False)
model_c = LinearRegression(fit_intercept=True)

def plot_for_each_data(data):
  zero_size_data = data[0]

  data -= zero_size_data

  x = np.array([i for i in range(0, 11)])

  model.fit(x.reshape((-1, 1)), data)

  data += zero_size_data

  plt.plot(x, data, label="static profiling results")
  plt.plot(x, model.predict(x.reshape((-1, 1))) +
           zero_size_data, label="linear regression model")

  plt.title(f"Runtime estimate for 'for each'")
  plt.xlabel("For each iterations")
  plt.ylabel("Runtime (ns)")

  plt.legend(loc="best")

  plt.show()

  print(f"for each\n  m: {model.coef_[0]}\n  c: {zero_size_data}\n\n")


def plot_par_data(data):
  x = np.array([i for i in range(2, 11)])

  model_c.fit(x.reshape((-1, 1)), data)

  plt.plot(x, data, label="static profiling results")
  plt.plot(x, model_c.predict(x.reshape((-1, 1))),
           label="linear regression model")

  plt.title(f"Runtime estimate for parallelisation")
  plt.xlabel("Number of parallel blocks")
  plt.ylabel("Runtime (ns)")

  plt.legend(loc="best")

  plt.show()

  print(f"parallelisation\n  m: {model_c.coef_[0]}\n  c: {model_c.intercept_}\n\n")


def plot_data(heading, data):

  if heading == "append":

    x = np.array([i ** 3 for i in range(len(data))]) 
    model_c.fit(x.reshape((-1, 1)), data)

    plt.plot(x, data, label="static profiling results")
    plt.plot(x, model_c.predict(x.reshape((-1, 1))), label="linear regression model")

    plt.title(f"Runtime estimate for {heading}")
    plt.xlabel("Input size")
    plt.ylabel("Runtime (ns)")

    plt.legend(loc="best")

    plt.show()

    print(f"{heading}\n  m: {model_c.coef_[0]}\n  c: {model_c.intercept_}\n\n")

  else: 
    zero_size_data = data[0]

    data -= zero_size_data

    x = np.array([i ** 3 for i in range(len(data))])

    model.fit(x.reshape((-1, 1)), data)

    data += zero_size_data

    plt.plot(x, data, label="static profiling results")
    plt.plot(x, model.predict(x.reshape((-1, 1))) +
            zero_size_data, label="linear regression model")

    plt.title(f"Runtime estimate for {heading}")
    plt.xlabel("Input size")
    plt.ylabel("Runtime (ns)")

    plt.legend(loc="best")

    plt.show()

    print(f"{heading}\n  m: {model.coef_[0]}\n  c: {zero_size_data}\n\n")


def analyse_results(results_file):
  with open(results_file) as file:
    reader_obj = csv.reader(file)

    no_lines = len(list(reader_obj))
    no_iters = int(no_lines / 7)

  with open(results_file) as file:
    reader_obj = csv.reader(file)

    simple_ops = ["empty", "assign", "var_read", "add", "mult", "concat",
                  "bool_cond", "increment", "if", "open", "func_call", "for_loop"]
    simple_ops_data = np.zeros(len(simple_ops))

    n = 75

    par_data = np.zeros(9)
    for_each = np.zeros(n)
    print_data = np.zeros(n)
    read_data = np.zeros(n)
    write_data = np.zeros(n)
    append_data = np.zeros(n)

    data_arrs = [simple_ops_data, par_data, for_each,
                 print_data, read_data, write_data, append_data]

    for _ in range(no_iters):
      for arr in data_arrs:
        data = next(reader_obj)
        data = np.array([float(item) for item in data])
        arr += data

    for arr in data_arrs:
      arr /= no_iters

    for i in range(len(simple_ops)):
      print(f"{simple_ops[i]}: {simple_ops_data[i]}")

    plot_par_data(data_arrs[1])
    # plot_for_each_data(data_arrs[2])

    long_ops = ["for loop", "print", "read", "write", "append"]

    for i in range(5):
      plot_data(long_ops[i], data_arrs[i + 2])


# empty := run_experiment(get_empty_runtime_cost, "empty", 0.)
# 	assign := run_experiment(get_assign_runtime_cost, "assign", empty)
# 	var_read := run_experiment(get_var_read_runtime_cost, "var read", empty+assign)
# 	add := run_experiment(get_add_runtime_cost, "add", empty+assign)
# 	mult := run_experiment(get_mult_runtime_cost, "mult", empty+assign)
# 	concat := run_experiment(get_concat_runtime_cost, "concat", empty+assign)
# 	bool_cond := run_experiment(get_bool_cond_runtime_cost, "bool cond", empty+assign)
# 	increment := run_experiment(get_increment_runtime_cost, "increment", empty)
# 	_if := run_experiment(get_if_statement_runtime_cost, "if statement", empty+increment+bool_cond)
# 	_open := run_experiment(get_open_runtime_cost, "open", empty+assign+var_read)
# 	func_call := run_experiment(get_func_call_runtime_cost, "func call", empty+var_read+add+assign)
# 	for_loop := run_experiment(get_for_loop_runtime_cost, "for loop", 0.)

  #   headings = next(reader_obj)

  #   for_each_data = [float(item) for item in next(reader_obj)]
  #   par_data = [float(item) for item in next(reader_obj)]

  #   data = []

  #   for row in reader_obj:
  #     data.append([float(item) for item in row])
  # return headings, np.array(for_each_data), np.array(par_data), np.array(data).transpose()
if __name__ == "__main__":
  results_file = "./results/results2.csv"

  analyse_results(results_file)

  # headings, for_each_data, par_data, data = get_data(results_file)

  # plot_for_each_data(for_each_data)
  # plot_par_data(par_data)

  # for heading, data in zip(headings, data):
  #   plot_data(heading, data)
