import pandas as pd
import numpy as np

raw = pd.read_excel("Confounding factors 14 yrs for 21 countries.xlsx",usecols="A:W")

raw = pd.DataFrame(raw)

raw = raw.loc[:74]

#now the dataframe is in correct shape, clean the left side column

df = raw

#here I deal with the formating in left col and fill all rows
df['Item'].fillna(method='ffill', inplace= True)

df = df.melt(id_vars=["year","Item"],var_name="Country",value_name="unit")

check = df
print(df)

check["country_year"] = check["Country"] + check["year"].astype(str)
check.drop(["Country","year"], axis = 1, inplace=True)

# df = df.astype(str) #convert all columns to strings as I have to combine numbers in the same cell

# df = df.replace('nan','') #get rid of the nan created back to a blank string

#check = pd.pivot(check, index="country_year",columns="Item", values="unit")
check = pd.pivot_table(check, index="country_year",columns="Item", values="unit")
check["Happiness"]
#print(check)


#check.to_excel("haha2.xlsx")
#later modified haha manually to rid of all ave rows