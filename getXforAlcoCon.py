import pandas as pd
import numpy as np

raw = pd.read_excel("protoX.xlsx") 

raw = pd.DataFrame(raw)

#for the 4 different Ys with different years

#1980 1990 1996 2006 2011 2012 2014 2018 are the Y years for alco consume
#90 95 00 05 10 16 alco prev
#80 96 06 11 12 14 smok consum 
#80 96 06 12 13 smok prev

#raw.drop(raw.index[[22,24,25,27,30,32]],inplace=True) alco consum
#raw.drop(raw.index[[20,23,26,28,29,30,31,33]],inplace=True) alco prev
#raw.drop(raw.index[[21,22,24,25,27,30,32,33]],inplace=True) smok consum
raw.drop(raw.index[[21,22,24,25,27,28,31,32,33]],inplace=True) #smok prev

#raw.sort_values(by=["Country|locus"],inplace=True)

extras = raw.loc[raw.index[:20].repeat(4)]

#raw.concat(extras)
#raw.append(extras,ignore_index=True)
raw = pd.concat([raw,extras], axis=0)

raw.sort_values(by=["Country|locus"],inplace=True)

#print(raw)
raw.to_excel("protoXforSmokPrev.xlsx")
#this has 21 countries. In Order to match, removed the 2 countries not present in Y(only 19) manually
#No bangladish, pakistan