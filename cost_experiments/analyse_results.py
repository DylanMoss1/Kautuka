import numpy as np
import matplotlib as plt
from csv import reader


class InstructionData:
    def __init__(self, name, data):
        self.name = name
        self.data = np.array([int(x) for x in data])

    def plot(self): 
        

def parse_data():
    instruction_data_list = []
    data_file = "./results/result.csv"

    with open(data_file, 'r') as f:
        csv_reader = reader(f)
        for row in csv_reader:
            instruction_data = InstructionData(row[0], row[1:])
            instruction_data_list.append(instruction_data)

    return instruction_data_list


def analyse_results(instruction_data):
    percentile_10 = np.percentile(instruction_data.data, 10) 
    percentile_90 = np.percentile(instruction_data.data, 90)




if __name__ == "__main__":
    analyse_results()
