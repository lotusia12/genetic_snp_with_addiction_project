from typing import TypeGuard
import pandas as pd
import numpy as np

#there are different ordering and different countries for each Y

raw = pd.read_excel("protoXforSmokPrev.xlsx") 

raw = pd.DataFrame(raw)
#print(raw.columns)

#alline the countries correctly

#code for alco consum
# raw["Unnamed: 0"].replace({19:34},inplace=True)
# raw.sort_values(by=["Unnamed: 0"],inplace=True)

#code for alco prev
# raw["Unnamed: 0"].replace({19:34},inplace=True)
# raw = raw[raw["Unnamed: 0"].isin([0,12]) == False]
# raw.sort_values(by=["Unnamed: 0"],inplace=True)


#code for smok cons
# raw["Unnamed: 0"].replace({19:34},inplace=True)
# raw.sort_values(by=["Unnamed: 0"],inplace=True)

#code for smok prev
raw["Unnamed: 0"].replace({19:34},inplace=True)
raw.sort_values(by=["Unnamed: 0"],inplace=True)


raw.to_excel("maybe3.xlsx",index=False)

raw2 = pd.read_excel("smokP+confound.xlsx",header=0) 

raw2 = pd.DataFrame(raw2)
#print(raw2)


use = pd.read_excel("maybe3.xlsx",header=0) 

use = pd.DataFrame(use)
#print(use)
allButEd = pd.concat([raw2, use], axis=1)
#print(allButEd)
allButEd.drop(["Unnamed: 0"],axis = 1, inplace = True)
allButEd.to_excel("oksp.xlsx")
# ok, okap, oksc, oksp
