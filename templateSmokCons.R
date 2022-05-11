#modify the code below for all 3 other Ys
library(readxl)
library(GGally)

use <- read_excel("withEdSmokCon.xlsx") #alcohol prevalence
use <- use[,-c(2,4)] #rid of unnamed col and fruit vege old


Y <- use[,c(2)]
X1 <- use[,c(13)]
conf <- use[,c(3:11,8358:8359)]
tot <- cbind(Y,X1,conf) #confounding except edu

tot <- subset(tot, select = -c(9))
tot <- subset(tot, select = -c(5,6,8))
tot$`GDP per capita` <- log(tot$`GDP per capita`)
names(tot)
#should be GDP, fruit, veggie, meat, temperature, 
#happiness, air pollution

ed <- use[,c(8360:8374)]

edp <- cbind(Y,ed)
edp <- subset(edp, select = -c(4,6,7))

sel <- edp[, c(4:8, 12:13)]
#check list here(too long, me can't type)
sel$mean <- rowMeans(sel, na.rm = TRUE) #the education mean
tot$ed_mean <- sel$mean

dev.off()
tot1 = tot[,-c(2)]
tot1$`GDP per capita` = exp(tot1$`GDP per capita`)
ggpairs(tot1,upper = list(continuous = "points"),lower = list(continuous = "cor"))

df <- tot
library(dplyr)

df <- df %>%
  rename(fruit = `Fruit
         yellow:2nd report(14,16,18)`)

df <- df %>%
  rename(gdp = `GDP per capita`)

df <- df %>%
  rename(temp = `yearly temperature`)

df <- df %>%
  rename(meat = `meat consumption`)

df <- df %>%
  rename(chr1_2156362 = `chr1:2156362`)

save_incase <- df

#imputations

#first use old
home <- read_excel("full_Imp_sim.xlsx")
library(tidyr)
use_sep <- use %>% separate(country_year, into = c('country', 'year'), sep = -4, convert = TRUE)
check <- cbind(tot[,-c(1)], use[,c(1:2,14:8358)]) #the unimputed data with all needed info
write.csv(check,"unimp_smok_cons.csv") #save in case

home_imp <- home[,3:12] #former imputed columns + country + year

check <- check[,-c(8356)]
check <- check %>% separate(country_year, into = c('country', 'year'), sep = -4, convert = TRUE)
save_incase <- check




check <- check %>%
  rename(fruit = `Fruit
yellow:2nd report(14,16,18)`)

check <- check %>%
  rename(gdp = `GDP per capita`)

check <- check %>%
  rename(temp = `yearly temperature`)

check <- check %>%
  rename(meat = `meat consumption`)

check$ed_mean[is.nan(check$ed_mean)]<-NA

.keys <- c("year","country")
df_result <- check %>%
  # Pull off rows from base table that match the join table
  semi_join(home_imp, .keys) %>%
  # Drop cols from base table that are in join table, except for the key columns
  select(-matches(setdiff(names(home_imp), .keys))) %>%
  # Left join on the join table columns
  left_join(home_imp, .keys) %>%
  # Remove the matching rows from the base table, and bind on the newly joined result from above.
  bind_rows(check %>% anti_join(home_imp, .keys))

df_result <- df_result[order(df_result$country),]


#alcohol prev has only overlap for 90, therefore all needs imputation
#check which has missing

df <- df_result
df <- filter(df, country != "Puerto Rico")

names(which(colSums(is.na(df))>0))

missDummy <- function(t)
{
  x <- dim(length(t)) 
  x[which(!is.na(t))] = 1
  x[which(is.na(t))] = 0
  return(x)
}

#meat

fit=loess(meat~gdp,span = 0.25, cell = 0.5, data=df)

plot(df$gdp, df$meat,main="meat consume by gdp fit")
mtext("Using loess smoothed fit to impute missing values")
fit=loess(meat~gdp, data=df)
Hpredict <- predict(fit,data.frame(gdp=df$gdp))
lines(df$gdp,Hpredict,col=i)

list <-which(is.na(df$meat))

points(df$gdp[list],Hpredict[list],col="red")

df$dummy <- missDummy(df$meat)

for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$meat[i] = Hpredict[i]
  }
}

which(is.na(df$meat))


#air

fit=loess(air_pollution~gdp,span = 0.25, cell = 0.5, data=df)

plot(df$gdp, df$air_pollution,main="air_pollution by gdp fit")
mtext("Using loess smoothed fit to impute missing values")
fit=loess(air_pollution~gdp, data=df)
Hpredict <- predict(fit,data.frame(gdp=df$gdp))
lines(df$gdp,Hpredict,col=i)

list <-which(is.na(df$air_pollution))
points(df$gdp[list],Hpredict[list],col="red")

df$dummy <- missDummy(df$air_pollution)

for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$air_pollution[i] = Hpredict[i]
  }
}

which(is.na(df$air_pollution))

#veggie

fit=loess(veggie~gdp,span = 0.25, cell = 0.5, data=df)

plot(df$gdp, df$veggie,main="veggie by gdp fit")
mtext("Using loess smoothed fit to impute missing values")
fit=loess(veggie~gdp, data=df)
Hpredict <- predict(fit,data.frame(gdp=df$gdp))
lines(df$gdp,Hpredict,col=i)

list <-which(is.na(df$veggie))
points(df$gdp[list],Hpredict[list],col="red")

df$dummy <- missDummy(df$veggie)

for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$veggie[i] = Hpredict[i]
  }
}

which(is.na(df$veggie))

write.csv(df, "imputed_smok_cons.csv")

imputed <-  read.csv("imputed_smok_cons.csv")

imputed_cl <- imputed[,c('smok_consum','gdp','fruit','veggie','meat','temp','Happiness','air_pollution','ed_mean')]

ggpairs(imputed_cl,upper = list(continuous = "points"),lower = list(continuous = "cor"))



#fit model
home <- df #transfer the imputed values to home
add <- read_excel("AC177_confounding_adjusted_annotated_11-30-2021.xlsx")
home$yearF <- 1:6 #6 years

df <- data.frame(matrix(ncol = 11, nrow = 8345))
colnames(df)<- c("name", "lower","est","upper","p_val","max_p","ave_p","BH_FDR","adjusted_p","r_squared","pearson_cor")


library(nlme)
use<-scale(home$smok_consum, scale = FALSE)
use2 <- as.data.frame(use)

v=1
for (i in colnames(home[,c(1,5:8348)])){
  df$name[v] = i
  
  x <- home[[i]]
  home$dum = x
  pos <- grep(i, add$locus)
  if (length(pos) != 0){
    df$max_p[v] <- add$max8[pos]
    df$ave_p[v] <- add$ave8[pos]
  }
  if (all(x==0)){
    
    df$p_val[v] <- NA
    df$lower[v] <- NA
    df$est[v] <- NA
    df$upper[v] <- NA
    
  } else { 
    fit.arh1<-gls(smok_consum~dum+gdp+fruit+veggie+meat+temp+Happiness+air_pollution+ed_mean,data=home,correlation=corAR1(form=~yearF|country),na.action = na.exclude)
    save <- summary(fit.arh1)
    df$p_val[v] <- save$tTable[2,4]
    ci <- as.data.frame(intervals(fit.arh1)[1])
    df$lower[v] <- ci$coef.lower[2]
    df$est[v] <- ci$coef.est.[2]
    df$upper[v] <- ci$coef.upper[2]
    df$r_squared[v] = 1-sum(fit.arh1$residuals^2)/sum(use2[-1,]^2)
    df$pearson_cor[v] = cor(home$smok_consum, fit.arh1$fitted)
  }
  v=v+1
}

library(FDRestimation)
save <-p.fdr(df$p_val)
save$`Results Matrix`
mod = save$`Results Matrix`[1]
library(berryFunctions)
ins <- insertRows(mod, 7566:7567, new = NA)
df$BH_FDR <- unlist(ins)

mod = save$`Results Matrix`[2]

ins <- insertRows(mod, 7566:7567, new = NA)
df$adjusted_p <- unlist(ins)

write.csv(df, "fitted_values_for_smok_consum.csv")









