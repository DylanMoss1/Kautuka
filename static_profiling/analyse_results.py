import csv
import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression

model = LinearRegression(fit_intercept=False)

def plot_for_each_data(data):
  zero_size_data = data[0]

  data -= zero_size_data

  x = np.array([i for i in range(0, 11)])

  model.fit(x.reshape((-1, 1)), data)

  data += zero_size_data

  plt.plot(x + 2, data, label="static profiling results")
  plt.plot(x + 2, model.predict(x.reshape((-1, 1))) +
           zero_size_data, label="linear regression model")

  plt.title(f"Runtime estimate for 'for each'")
  plt.xlabel("For each iterations")
  plt.ylabel("Runtime (ns)")

  plt.legend(loc="best")

  plt.show()

  print(f"for each\n  m: {model.coef_[0]}\n  c: {zero_size_data}\n\n")
  


def plot_par_data(data):
  two_size_data = data[0]

  data -= two_size_data

  x = np.array([i for i in range(0, 9)])

  model.fit(x.reshape((-1, 1)), data)

  data += two_size_data

  plt.plot(x + 2, data, label="static profiling results")
  plt.plot(x + 2, model.predict(x.reshape((-1, 1))) +
           two_size_data, label="linear regression model")

  plt.title(f"Runtime estimate for parallelisation")
  plt.xlabel("Number of parallel blocks")
  plt.ylabel("Runtime (ns)")

  plt.legend(loc="best")

  plt.show()

  print(f"parallelisation\n  m: {model.coef_[0]}\n  c: {two_size_data}\n\n")


def plot_data(heading, data):

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


def get_data(results_file):

  with open(results_file) as file:
    reader_obj = csv.reader(file)

    headings = next(reader_obj)
  
    for_each_data = [float(item) for item in next(reader_obj)]
    par_data = [float(item) for item in next(reader_obj)]

    data = []

    for row in reader_obj:
      data.append([float(item) for item in row])

  return headings, np.array(for_each_data), np.array(par_data), np.array(data).transpose()


if __name__ == "__main__":
  results_file = "./results/results.csv"

  headings, for_each_data, par_data, data = get_data(results_file)

  plot_for_each_data(for_each_data)
  plot_par_data(par_data)

  for heading, data in zip(headings, data):
    plot_data(heading, data)
