import pandas as pd
import numpy as np

raw = pd.read_excel("education_factors_21countries_v2.xlsx") 

raw = pd.DataFrame(raw)

raw.drop(["Country Code"], inplace= True, axis=1)
raw = raw.melt(id_vars=["Country Name","Indicator Name"],var_name="year",value_name="Ed")
#print(raw)
check = pd.pivot_table(raw, index=["Country Name", "year"],columns="Indicator Name", values="Ed")
check.to_excel("edFactors.xlsx")