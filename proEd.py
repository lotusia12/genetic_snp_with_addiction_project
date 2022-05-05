import pandas as pd
import numpy as np

#change the ok part for 4 Ys
raw = pd.read_excel("edFactors.xlsx") 

check = pd.DataFrame(raw)
check['Country Name'].fillna(method='ffill', inplace= True)


check["country_year"] = check["Country Name"] + check["year"].astype(str)
check.drop(["Country Name","year"], axis = 1, inplace=True)

#print(check)


raw2 = pd.read_excel("oksp2.xlsx") 

raw2 = pd.DataFrame(raw2)

joined = raw2.set_index("country_year").join(check.set_index("country_year"))
#print(joined)
joined.to_excel("withEdSmokPrev.xlsx")