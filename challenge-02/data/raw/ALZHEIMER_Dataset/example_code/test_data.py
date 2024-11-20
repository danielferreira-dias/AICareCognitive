import pandas as pd

# Relative path to the dataset
file_path = "../Dataset/alzheimers_disease_data.csv"

# Load the dataset
data = pd.read_csv(file_path)

# Display the first few rows
print(data.head())