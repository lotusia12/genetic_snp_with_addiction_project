import pandas as pd
import numpy as np

#change the ok part for 4 Ys

raw = pd.read_excel("vegefruit.xlsx")
raw = pd.DataFrame(raw)

df = raw

#here I deal with the formating in left col and fill all rows
df['Item'].fillna(method='ffill', inplace= True)

df = df.melt(id_vars=["year","Item"],var_name="Country",value_name="unit")

check = df
#print(df)

check["country_year"] = check["Country"] + check["year"].astype(str)
check.drop(["Country","year"], axis = 1, inplace=True)

# df = df.astype(str) #convert all columns to strings as I have to combine numbers in the same cell

# df = df.replace('nan','') #get rid of the nan created back to a blank string

#check = pd.pivot(check, index="country_year",columns="Item", values="unit")
check = pd.pivot_table(check, index="country_year",columns="Item", values="unit")

#print(check)


raw2 = pd.read_excel("oksp.xlsx", index_col="country_year") 
#raw2 = pd.read_excel("okap.xlsx") 

raw2 = pd.DataFrame(raw2)

#print(raw2.columns, check.columns)
joined = raw2.join(check)

joined.to_excel("oksp2.xlsx")