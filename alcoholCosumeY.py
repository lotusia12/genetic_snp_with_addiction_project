import pandas as pd
import numpy as np

#this code will be modified to process all 4 Ys 
raw = pd.read_excel("smoking_consumption.xlsx") 

raw = pd.DataFrame(raw)
#print(raw)

check = raw.melt(id_vars="Country",var_name="year",value_name="smok_consum")
#print(check["Country"])
check.sort_values(by=["Country","year"], inplace=True)
#print(check)
check["Country"].replace({"United States of America":"USA", "United Kingdom":"UK"},inplace=True)
check["country_year"] = check["Country"] + check["year"].astype(str)
check.drop(["Country","year"], axis = 1, inplace=True)
check = check[["country_year","smok_consum"]]
#print(check)

check.to_excel("smokConsumY.xlsx", index=None)
#print(check)