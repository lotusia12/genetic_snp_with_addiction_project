rm(list = ls())

library(readxl)
library(magrittr)
library(xlsx)
library(GGally)

library(dplyr)

use <- read_excel("ok2.xlsx")

#split country year again
library(tidyr)
use <- use %>% separate(country_year, into = c('country', 'year'), sep = -4, convert = TRUE)

Y <- use[,c(2)]
X1 <- use[,c(13)]
conf <- use[,c(3:11,8358:8359)]
tot <- cbind(Y,X1,conf)
#no percip(only 2017)



tot <- subset(tot, select = -c(9))
names(tot)
tot <- subset(tot, select = -c(5,6,8))

tot$test <- log(tot$`GDP per capita`)

#if (tot$air_pollution > 40):
  #tot$dum = 0;

plot(use$air_pollution, use$alcohol_consum)
plot((use$`GDP per capita`),log(use$air_pollution))
use$gdp2 <- use$`GDP per capita`^2
zz<- lm(log(use$air_pollution)~use$`GDP per capita`+ use$gdp2)



ggpairs(tot,upper = list(continuous = "points"),lower = list(continuous = "cor"))

names(tot)
names(edp)

#Variable distribution is available on the diagonal.

use <- read_excel("withEd.xlsx")
ed <- use[,c(8360:8374)]

edp <- cbind(Y,ed)
edp <- subset(edp, select = -c(4,6,7))

edp$lg_ <- log(edp$`Educational attainment, at least completed short-cycle tertiary, population 25+, total (%) (cumulative)`)

ggpairs(edp,upper = list(continuous = "points"),lower = list(continuous = "cor"))

new_set <- subset(tot, select = -c(10,11))
new_set$ed_imputed <- edp$imputed_value
edp$mean <- rowMeans(edp, na.rm = TRUE)

#log the gdp here
new_set$`GDP per capita` = log(new_set$`GDP per capita`)

#replace air pollution with 2 variables
new_set$air_pollution_above40 <- new_set$air_pollution
new_set$air_pollution[new_set$air_pollution > 40] <- 0

new_set$air_pollution_above40[new_set$air_pollution_above40 <= 40] <- 0

new_set$country <- use$country 
new_set$year <- use$year

library(dplyr)
new_set <- data.frame(new_set)
test <- aov(alcohol_consum ~ chr1.2156362 + GDP.per.capita + Happiness
            + meat.consumption +yearly.temperature +air_pollution +Fruit..yellow.2nd.report.14.16.18.,data = new_set)


test <- lm(alcohol_consum ~ chr1.2156362 + GDP.per.capita^2 + Happiness^2
+ meat.consumption^2+yearly.temperature^2                
+air_pollution^2 +Fruit..yellow.2nd.report.14.16.18.^2
+veggie^2+air_pollution_above40^2,data = new_set)



#find na's and drop(in this case puerto rico)
which(is.na(new_set$alcohol_consum))
full_set <- new_set[-c(97:104),]

#impute with lm

#do the edu one

symnum(cor(edp, use = "complete.obs"))

edp$dummy <- missDummy(edp$`Educational attainment, at least completed short-cycle tertiary, population 25+, total (%) (cumulative)`)

#this part impute the missing values with mean
library(Hmisc)

edp$imputed_value <- with(edp, impute(`Educational attainment, at least completed short-cycle tertiary, population 25+, total (%) (cumulative)`, mean))

lm(edp$`Educational attainment, at least completed short-cycle tertiary, population 25+, total (%) (cumulative)` ~ ., edp)

for(i in 1:nrow(full_set))
{
  if(full_set$dummy[i] == 0)
  {
    full_set$Happiness[i] = 1.5863 + 0.4879*full_set$`GDP per capita`[i]
  }
}





missDummy <- function(t)
{
  x <- dim(length(t)) 
  x[which(!is.na(t))] = 1
  x[which(is.na(t))] = 0
  return(x)
}

#for happ
full_set$dummy <- missDummy(full_set$Happiness)

lm(Happiness ~ full_set$`GDP per capita`, full_set)

for(i in 1:nrow(full_set))
{
  if(full_set$dummy[i] == 0)
  {
    full_set$Happiness[i] = 1.5863 + 0.4879*full_set$`GDP per capita`[i]
  }
}
#meat
full_set$dummy <- missDummy(full_set$`meat consumption`)

lm(`meat consumption` ~ full_set$`GDP per capita`, full_set)

for(i in 1:nrow(full_set))
{
  if(full_set$dummy[i] == 0)
  {
    full_set$`meat consumption`[i] = -83.98 + 16.36*full_set$`GDP per capita`[i]
  }
}
#air(2)
full_set$dummy <- missDummy(full_set$air_pollution)

lm(air_pollution ~ full_set$`GDP per capita`, full_set)

for(i in 1:nrow(full_set))
{
  if(full_set$dummy[i] == 0)
  {
    full_set$air_pollution[i] = 29.479 - 1.434*full_set$`GDP per capita`[i]
  }
}
full_set$dummy <- missDummy(full_set$air_pollution_above40)

lm(air_pollution_above40 ~ full_set$`GDP per capita`, full_set)

for(i in 1:nrow(full_set))
{
  if(full_set$dummy[i] == 0)
  {
    full_set$air_pollution_above40[i] = 59.695 - 5.449*full_set$`GDP per capita`[i]
  }
}

#veggie

full_set$dummy <- missDummy(full_set$veggie)

lm(veggie ~ full_set$`GDP per capita`, full_set)

for(i in 1:nrow(full_set))
{
  if(full_set$dummy[i] == 0)
  {
    full_set$veggie[i] =0.4291 + 0.0459*full_set$`GDP per capita`[i]
  }
}
full_set <- subset(full_set, select = -c(ed_short_cycle,dummy))

write.csv(full_set,"imputed.csv")

#now fit model
test <- lm(alcohol_consum ~ full_set$`chr1:2156362` + full_set$`GDP per capita`^2 + Happiness^2
           + full_set$`meat consumption`^2+full_set$`yearly temperature`^2                
           +air_pollution^2 +full_set$`Fruit
yellow:2nd report(14,16,18)`^2
           +veggie^2+air_pollution_above40^2 + full_set$ed_imputed,data = full_set)

full_set <- full_set %>%
  rename(fruit = `Fruit
yellow:2nd report(14,16,18)`)

full_set <- full_set %>%
  rename(log_gdp = `GDP per capita`)

ggpairs(subset(full_set, select = -c(country, year)),upper = list(continuous = "points"),lower = list(continuous = "cor"))



rep <- aov(alcohol_consum ~ full_set$`chr1:2156362` + full_set$log_gdp^2 + Happiness^2
           + full_set$`meat consumption`^2+full_set$`yearly temperature`^2                
           +air_pollution^2 + full_set$fruit^2
           +veggie^2+air_pollution_above40^2 + full_set$ed_imputed,data = full_set)


fit.arh1 <- gls(pulse ~ exertype * time, data = longg,
                corr = corAR1(, form = ~ 1 | id), weight = varIdent(form = ~ 1 | time))
summary(fit.arh1)

write.csv(new_set,"raw_without_ed.csv")

sel <- edp[, c(4:8, 12:13)]
sel$mean <- rowMeans(sel, na.rm = TRUE)




