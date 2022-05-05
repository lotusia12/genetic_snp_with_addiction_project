import pandas as pd
import numpy as np

raw = pd.read_excel("WorldBank_airpollution.xlsx") 

raw = pd.DataFrame(raw)

#drop all empty cols
raw.dropna(how='all', axis=1, inplace=True)

check = raw.melt(id_vars="Country Name",var_name="year",value_name="air_pollution")
check.sort_values(by=["Country Name"], inplace=True)
#print(check)
check["Country Name"].replace({"United States":"USA", "United Kingdom":"UK","Vietnam":"Viet Nam"},inplace=True)

check["country_year"] = check["Country Name"] + check["year"].astype(str)
check.drop(["Country Name","year"], axis = 1, inplace=True)
check = check[["country_year","air_pollution"]]

check.to_excel("airPollution.xlsx")

print(check)