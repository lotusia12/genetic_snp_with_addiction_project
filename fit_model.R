raw <- read_xlsx("ok2.xlsx")
imputed <-  read.csv("impute_progress.csv")

imputed_cl <- imputed[,-c(1,2,12,13,15)]
imputed_cl1 <- imputed_cl[,-c(2)]
imputed_cl1$gdp=exp(imputed_cl1$gdp)
ggpairs(imputed_cl1,upper = list(continuous = "points"),lower = list(continuous = "cor"))


use <- raw %>% separate(country_year, into = c('country', 'year'), sep = -4, convert = TRUE)

library(dplyr)

df= imputed %>% inner_join(use,by=c("country"="country", "year"="year"))

df_simp <- df[,-c(15:26)]
df_simp <- df_simp[,-c(1,2)]

df <- df_simp


df <- df %>%
  rename(Happiness = Happiness.x)

df <- df %>%
  rename(alcohol_consum = alcohol_consum.x)

df <- df %>%
  rename(veggie = veggie.x)

df <- df %>%
  rename(air_pollution = air_pollution.x)

write.csv(df,"full_Imp.csv")

#modify some overlap column names 

library(readxl)

home <- read.csv("full_Imp_sim.csv")

home <- read_excel("full_Imp_sim.xlsx")
add <- read_excel("AC177_confounding_adjusted_annotated_11-30-2021.xlsx")
library(glmmTMB)

#dfjoin = home %>% inner_join(add,by=c("locus"="year"))


#ggplot
pluse <- imputed[,-c(1:2,12:13,15)]
ggpairs(pluse,upper = list(continuous = "points"),lower = list(continuous = "cor"))

#ar(1) with zero mean fit
fit <- glmmTMB(alcohol_consum ~ -1 + ar1(as.factor(year) + 0|country), data = home)
library(nlme)

home$yearF <- 1:8
#home$yearF <- as.numeric(home$year)
#fit.arh1<-gls(alcohol_consum~gdp+fruit+veggie+meat+temp+Happiness+temp+air_pollution+ed_mean,data=home,correlation=corAR1(form=~year|as.factor(country)),na.action = na.exclude)
df <- data.frame(matrix(ncol = 11, nrow = 8345))
colnames(df)<- c("name", "lower","est","upper","p_val","max_p","ave_p","BH_FDR","adjusted_p","r_squared","pearson_cor")

v=1
for (i in colnames(home)[13:8357]){
  tryCatch(
    expr={
  df$name[v] = i
  #print(i)
  #print(get(i))
  x <- home[[i]]
  home$dum = x
  fit.arh1<-gls(alcohol_consum~dum+gdp+fruit+veggie+meat+temp+Happiness+temp+air_pollution+ed_mean,data=home,correlation=corAR1(form=~yearF|country),na.action = na.exclude)
  save <- summary(fit.arh1)
  df$p_val[v] <- save$tTable[2,4]
  ci <- as.data.frame(intervals(fit.arh1)[1])
  df$lower[v] <- ci$coef.lower[2]
  df$est[v] <- ci$coef.est.[2]
  df$upper[v] <- ci$coef.upper[2]
  v=v+1},
  error=function(e){
    df$name[v] = i
    df$p_val[v] <- NA
    
    df$lower[v] <- NA
    df$est[v] <- NA
    df$upper[v] <- NA
    v=v+1
  }
  )
  }
v=1
for (i in colnames(home)[13:8357]){
  tryCatch(
    expr={
      df$name[v] = i
      #print(i)
      #print(get(i))
      x <- home[[i]]
      home$dum = x
      fit.arh1<-gls(alcohol_consum~dum+gdp+fruit+veggie+meat+temp+Happiness+temp+air_pollution+ed_mean,data=home,correlation=corAR1(form=~yearF|country),na.action = na.exclude)
      save <- summary(fit.arh1)
      df$p_val[v] <- save$tTable[2,4]
      ci <- as.data.frame(intervals(fit.arh1)[1])
      df$lower[v] <- ci$coef.lower[2]
      df$est[v] <- ci$coef.est.[2]
      df$upper[v] <- ci$coef.upper[2]
      v=v+1},
    error=function(e){
      df$name[v] = i
      df$p_val[v] <- NA
      
      df$lower[v] <- NA
      df$est[v] <- NA
      df$upper[v] <- NA
      v=v+1
    }
  )
}


use<-scale(home$alcohol_consum, scale = FALSE)
use2 <- as.data.frame(use)

1-sum(fit.arh1$residuals^2)/sum(use2[-1,]^2)
v=1
for (i in colnames(home)[13:8357]){
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
      fit.arh1<-gls(alcohol_consum~dum+gdp+fruit+veggie+meat+temp+Happiness+air_pollution+ed_mean,data=home,correlation=corAR1(form=~yearF|country),na.action = na.exclude)
      save <- summary(fit.arh1)
      df$p_val[v] <- save$tTable[2,4]
      ci <- as.data.frame(intervals(fit.arh1)[1])
      df$lower[v] <- ci$coef.lower[2]
      df$est[v] <- ci$coef.est.[2]
      df$upper[v] <- ci$coef.upper[2]
      df$r_squared[v] = 1-sum(fit.arh1$residuals^2)/sum(use2[-1,]^2)
      df$pearson_cor[v] = cor(home$alcohol_consum[-1], fit.arh1$fitted)
      }
      v=v+1
}

df <- as.data.frame(df)

fit.arh1<-gls(alcohol_consum~chr1.2156362+gdp+fruit+veggie+meat+temp+Happiness+air_pollution+ed_mean,data=home,correlation=corAR1(form=~yearF|country),na.action = na.exclude)
save <- summary(fit.arh1)
p_val <- save$tTable[2,4]
ci <- as.data.frame(intervals(fit.arh1)[1])
low <- ci$coef.lower[2]
est <- ci$coef.est.[2]
up <- ci$coef.upper[2]
rsquared.gls(fit.arh1)

test = "chr1.2156362"
library(piecewiseSEM)
use<-scale(home$alcohol_consum, scale = FALSE)
use2 <- as.data.frame(use)

1-sum(fit.arh1$residuals^2)/sum(use2[-1,]^2)


fit.arh2<-gls(alcohol_consum~chr1.2156362+gdp,data=home,correlation=corAR1(form=~yearF|country),na.action = na.exclude)
save <- summary(fit.arh2)
p_val <- save$tTable[2,4]
ci <- as.data.frame(intervals(fit.arh1)[1])
low <- ci$coef.lower[2]
est <- ci$coef.est.[2]
up <- ci$coef.upper[2]

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

write.csv(df,"listed_fit.csv")
library(writexl)
write_xlsx(df,"fit_table.xlsx")

test = data.frame(matrix(ncol = 8, nrow = 18))

homesm <- home[,c("yearF", "alcohol_consum")]
View(homesm)
check <- split(homesm, homesm$yearF)
list <- data.frame()
 View(check)
 
test[,1] <- check$`1`$alcohol_consum
test[,2] <- check$`2`$alcohol_consum
test[,3] <- check$`3`$alcohol_consum
test[,4] <- check$`4`$alcohol_consum
test[,5] <- check$`5`$alcohol_consum
test[,6] <- check$`6`$alcohol_consum
test[,7] <- check$`7`$alcohol_consum
test[,8] <- check$`8`$alcohol_consum

 cor(check, na.action=na.omit)
 test <- as.data.frame(test)
 ggpairs(test,upper = list(continuous = "points"),lower = list(continuous = "cor"))
 


