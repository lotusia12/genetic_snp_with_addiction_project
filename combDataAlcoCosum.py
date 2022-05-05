import pandas as pd
import numpy as np

#modify for 4 occasions

#raw = pd.read_excel("alcoCosumY.xlsx") 
#raw = pd.read_excel("alcoPrevY.xlsx") 
#raw = pd.read_excel("smokConsumY.xlsx") 
raw = pd.read_excel("smokPrevY.xlsx") 

raw = pd.DataFrame(raw)

#raw = raw.sort_values(by = ["country_year"])
#print(raw)

raw2 = pd.read_excel("haha.xlsx") 

raw2 = pd.DataFrame(raw2)

raw3 = pd.read_excel("airPollution.xlsx") 

raw3 = pd.DataFrame(raw3)

#print(raw3,raw3.columns)

joined = raw.set_index("country_year").join(raw2.set_index("country_year"))
#print(joined)
jojo = joined.join(raw3.set_index("country_year"))

print(jojo)
jojo.drop(["Unnamed: 0"],axis=1,inplace=True)
#print(joined)
jojo.to_excel("smokP+confound.xlsx")

#Now the set has Y:alcohol consumption, confounding factors with air pollution (NO education)

#next:add X