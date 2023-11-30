import pandas
import pyreadr
import os

print(os.getcwd())

enpol = pyreadr.read_r('/National/Data_cleaning/Output/Main_database.RData')
print(enpol)
#enpol = pyreadr.read_r('/National/Data_cleaning/Output/Main_database.RData')
