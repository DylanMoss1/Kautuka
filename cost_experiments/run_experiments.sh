#!/bin/bash

# Setup the experiment 

echo "Run setup..."
python setup.py 

# Run the experiments

iterations = 1000 
sleep_seconds = 10 

echo "Running $iterations iterations with $sleep_seconds(s) of sleep..."

counter = 0 

while [ $counter -le $iterations ]
do 
  go run test2.go 
  sleep $sleep_seconds 
done 

# Analyse the results 

echo "Analyse results..."

python analyse_results.py 

echo "Done"