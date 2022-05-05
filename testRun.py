import pandas as pd
import numpy as np
import openpyxl
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors

print("test")

#read in the diff gene stuff with two headers, country->years
raw = pd.read_excel("list of selected allele freq.xlsx", header=[0,1]) 

raw = pd.DataFrame(raw)
#print(raw.head())
#print(raw.keys())
# raw2 = raw.reindex(sorted(raw.keys()), axis=1)
# print(raw2.head())
# print(raw2.keys())

#read additional gene data(first sheet is repeated, so skip)
addition = pd.read_excel("additional_35_markers.xlsx", sheet_name= "Positives",header=[0,1])
addition = pd.DataFrame(addition)
#print(addition.keys())

#append the two charts based on country and year
#test = pd.concat([raw.loc[key] for key in addition.keys()], axis=0)
test = raw.append(addition)
#print(test.columns[1])
#test.columns = test.columns.astype(str)

#rid of multiindex
test = test.drop([('Country','Chr'),('Country','location')],axis=1)
test = test.set_axis(['|'.join(c) for c in test.columns], axis='columns', inplace=False)
#print(test.loc[:,test.columns != "Country|locus"].columns)

use = test.T
#print(use)

use.to_excel("protoX.xlsx", header=None) #prototype of desired format, with countries and gene stuff(x)


#test = test.pivot(columns="Country|locus",values=['Bangladesh|BEB', 'Barbados|ACB', 'China|CHB+CNS', 'Colombia|CLM','Finland|FIN', 'Gambia|GWD', 'India|GIH+ITU', 'Italy|TSI', 'Japan|JPT','Kenya|LWK', 'Mexico|MXL', 'Nigeria|ESN+YRI', 'Pakistan|PJL','Peru|PEL', 'Puerto Rico|PUR', 'Sierra Leone|MSL', 'Spain|IBS','Sri Lanka|STU', 'United Kingdom|GBR', 'Viet Nam|KHV','USA of different years|,1980', 'USA of different years|,1990','USA of different years|,1995', 'USA of different years|,1996','USA of different years|,2000', 'USA of different years|,2005','USA of different years|,2006', 'USA of different years|,2010','USA of different years|,2011', 'USA of different years|,2012','USA of different years|,2013', 'USA of different years|,2014','USA of different years|,2016', 'USA of different years|,2018'])
#test = test.groupby("Country|locus")
#print(test)
# test = pd.melt(test)

#print(test)
#test.to_excel("testWith1level.xlsx",index=False)
