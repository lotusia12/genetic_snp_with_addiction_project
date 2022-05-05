#small edit for ad

edit <- read_excel("fitted_values_for_smok_consum.xlsx")

ad <- read_excel("Pmax200.xlsx", "Tobacco consumption")

edit$Pmax = NA
for (v in 1:8345){
  i = edit$name[v]
  
  pos <- grep(i, ad$location)
  if (length(pos) != 0){
    edit$Pmax[v] = ad$Pmax[pos]
  }
}
write.csv(edit, "Tobacco_consumption_results_edited.csv")

