import numpy as np
from csv import reader


class Data:
    def __init__(self):
        self.instruction_data = np.array([])

    def add_data(self, data):
        self.instruction_data = np.append(self.instruction_data, data)

    def find_mean(self):
        return np.mean(self.instruction_data)

    def find_median(self):
        return np.median(self.instruction_data)

    def find_quartiles(self):
        return np.percentile(self.instruction_data, 25), np.percentile(self.instruction_data, 75)

    def find_percentiles(self):
        return np.percentile(self.instruction_data, 10), np.percentile(self.instruction_data, 90)

    def find_bounds(self):
        return np.min(self.instruction_data), np.max(self.instruction_data)


class InstructionCost:
    def __init__(self, name):
        self.name = name
        self.data = Data()

    def pprint_full(self):
        print("Name: {}".format(self.name))
        print("Data: {}".format(self.data))

    def pprint(self):
        print("Name: {}".format(self.name))

        lower_bound, upper_bound = self.data.find_bounds()
        lower_percentile, upper_percentile = self.data.find_percentiles()
        lower_quartile, upper_quartile = self.data.find_quartiles()
        median = self.data.find_median()

        print("Lower Bound: {}".format(lower_bound))
        print("Lower Percentile: {}".format(lower_percentile))
        print("Lower Quartile: {}".format(lower_quartile))
        print("Median: {}".format(median))
        print("Upper Quartile: {}".format(upper_quartile))
        print("Upper Percentile: {}".format(upper_percentile))
        print("Upper Bound: {}".format(upper_bound))


def parse_data():
    instruction_costs = []
    data_file = "./results/results.csv"

    with open(data_file, 'r') as f:
        csv_reader = reader(f)
        first_line = next(csv_reader)
        for instruction in first_line:
            instruction_costs.append(InstructionCost(instruction))

        for row in csv_reader:
            for i in range(len(row)):
                instruction_costs[i].data.add_data(int(row[i]))

    return instruction_costs

# def plot(instruction_costs):
#     # Create figure
#     fig = plt.figure()
#     ax = fig.add_subplot(111)

#     # Create an array of the data to be plotted
#     data = np.array([instruction.data for instruction in instruction_costs])

#     print(data)
#     print([instruction.name for instruction in instruction_costs])

#     # Create the boxplot
#     ax.boxplot(data)

#     # Add labels to the graph
#     ax.set_xticklabels([instruction.name for instruction in instruction_costs])
#     ax.set_ylabel("Runtime Cost")

#     # Save figure
#     fig.savefig("./results/boxplot.png")


def pprint_instruction_costs(instruction_costs):
    for instruction in instruction_costs:
        instruction.pprint()
        print()


if __name__ == "__main__":
    instruction_costs = parse_data()
    pprint_instruction_costs(instruction_costs)

    # plot(instruction_costs)
