setwd("C:/Users/lotus/Desktop/proj")

raw <- read.csv("raw_with_ed.csv")
#raw$ed_mean <- sel$mean


library(predict3d)
library(spatialEco)

library(dplyr)

#remove Puerto rico because it has no response var
df <- filter(raw, country != "Puerto Rico")

df <- df %>%
  rename(fruit = Fruit.yellow.2nd.report.14.16.18.)

df <- df %>%
  rename(gdp = GDP.per.capita)

df <- df %>%
  rename(temp = yearly.temperature)

df <- df %>%
  rename(meat = meat.consumption)

df <- df %>%
  rename(chr1_2156362 = chr1.2156362)

save_incase <- df

df <- save_incase

missDummy <- function(t)
{
  x <- dim(length(t)) 
  x[which(!is.na(t))] = 1
  x[which(is.na(t))] = 0
  return(x)
}


df$air_pollution[df$air_pollution==0] <- NA
df$air_pollution_above40[df$air_pollution_above40==0] <- NA
df$air_pollution[!is.na(df$air_pollution_above40)] = df$air_pollution_above40[!is.na(df$air_pollution_above40)] 

check <- df[, -c(1,2,12,13,14)]
check1 <- check[,-c(2)]

ggpairs(check1,upper = list(continuous = "points"),lower = list(continuous = "cor"))


#example
# Plot points on noisy curve
plot(x,y, main="Sine Curve + 'Uniform' Noise")
mtext("Using loess smoothed fit to impute missing values")
y.loess <- loess(y ~ x, span=0.75, data.frame(x=x, y=y))
y.predict <- predict(y.loess, data.frame(x=FullList))
# Plot the loess smoothed curve showing gaps for missing data
lines(x,y.predict,col=i)
# Show imputed points to fill in gaps
y.Missing <- predict(y.loess, data.frame(x=MissingList))
points(MissingList, y.Missing, pch=FILLED.CIRCLE<-19, col=i)



#hap

plot(df$gdp, df$Happiness,main="happiness by gdp fit")
mtext("Using loess smoothed fit to impute missing values")
fit=loess(Happiness~gdp, data=df)
Hpredict <- predict(fit,data.frame(gdp=df$gdp))
lines(df$gdp,Hpredict,col=i)

list <-which(is.na(df$Happiness))

#y.Missing <-  predict(y.loess, data.frame(x=MissingList))
points(df$gdp[list],Hpredict[list],col="red")


df$dummy <- missDummy(df$Happiness)

for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$Happiness[i] = Hpredict[i]
  }
}

see <- ggPredict(fit)

imp

which(is.na(df$Happiness))
df$gdp2 = df$gdp^2
hap=lm(Happiness~gdp+gdp2, data=df)

df$dummy <- missDummy(df$Happiness)

for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$Happiness[i] = 4.00161-0.12422*df$gdp[i] + 0.03701*df$gdp2[i]
  }
}

#meat


df$cou <- factor(df$country)
df$meat_cou <- df$meat

fit=loess(meat~gdp,span = 0.25, cell = 0.5, data=df)


plot(df$gdp, df$meat,main="meat consume by gdp fit")
mtext("Using loess smoothed fit to impute missing values")
fit=loess(meat~gdp, data=df)
Hpredict <- predict(fit,data.frame(gdp=df$gdp))
lines(df$gdp,Hpredict,col=i)

list <-which(is.na(df$meat))

#y.Missing <-  predict(y.loess, data.frame(x=MissingList))
points(df$gdp[list],Hpredict[list],col="red")



df$dummy <- missDummy(df$meat)

for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$meat[i] = Hpredict[i]
  }
}


ggPredict(fit)

which(is.na(df$meat))

hap=lm(meat~gdp+gdp2+gdp3,data=df)
save <- predict(hap)

df$dummy <- missDummy(df$meat)
list <-which(is.na(df$meat))


for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$meat[i] = save[i]
  }
}
points(df$gdp[list],save[list],col="red")


hap2=lm(meat~gdp+gdp2,data=df)


for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$meat[i] = -58.3860+9.4110*df$gdp[i] + 0.4467*df$gdp2[i]
  }
}

#air_pollution

fit=loess(air_pollution_above40~gdp,span=0.75,data=df)
ggPredict(fit)


plot(df$gdp, df$air_pollution_above40,main=">40 air polltion by gdp fit")
mtext("Using loess smoothed fit to impute missing values")
#fit=loess(air_polltion_above40~gdp, data=df)
Hpredict <- predict(fit,data.frame(gdp=df$gdp))
lines(df$gdp,Hpredict,col=i)

list <-which(is.na(df$air_pollution_above40))

#y.Missing <-  predict(y.loess, data.frame(x=MissingList))
points(df$gdp[list],Hpredict[list],col="red")



df$dummy <- missDummy(df$air_pollution_above40)

for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$air_pollution_above40[i] = Hpredict[i]
  }
}

fit=loess(air_pollution~gdp,span=0.75,data=df)
ggPredict(fit)


plot(df$gdp, df$air_pollution,main=" air polltion by gdp fit")
mtext("Using loess smoothed fit to impute missing values")
#fit=loess(air_polltion_above40~gdp, data=df)
Hpredict <- predict(fit,data.frame(gdp=df$gdp))
lines(df$gdp,Hpredict,col=i)

list <-which(is.na(df$air_pollution))

#y.Missing <-  predict(y.loess, data.frame(x=MissingList))
points(df$gdp[list],Hpredict[list],col="red")



df$dummy <- missDummy(df$air_pollution)

plot( use$air_pollution,use$`chr1:2156362`)

for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$air_pollution[i] = Hpredict[i]
  }
}

which(is.na(df$air_pollution))
df$gdp2 = df$gdp^2
hap=lm(Happiness~gdp+gdp2,data=df)

df$dummy <- missDummy(df$Happiness)

for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$Happiness[i] = 4.00161-0.12422*df$gdp[i] + 0.03701*df$gdp2[i]
  }
}

#veggie

fit=loess(veggie~gdp,span=0.5,data=df)
ggPredict(fit)


plot(df$gdp, df$veggie,main="veggie by gdp fit")
mtext("Using loess smoothed fit to impute missing values")
#fit=loess(air_polltion_above40~gdp, data=df)
Hpredict <- predict(fit,data.frame(gdp=df$gdp))
lines(df$gdp,Hpredict,col=i)

list <-which(is.na(df$veggie))

#y.Missing <-  predict(y.loess, data.frame(x=MissingList))
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
df$gdp2 = df$gdp^2
df$gdp3 = df$gdp^3
hap=lm(veggie~gdp+gdp2+gdp3,data=df)

df$dummy <- missDummy(df$veggie)

for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$veggie[i] = 4.00161-0.12422*df$gdp[i] + 0.03701*df$gdp2[i]
  }
}

#education mean
df$ed_mean <- use$mean

fit=loess(ed_mean~gdp,span=0.5,data=df)
ggPredict(fit)


plot(df$gdp, df$ed_mean,main="ed_mean by gdp fit")
mtext("Using loess smoothed fit to impute missing values")
#fit=loess(air_polltion_above40~gdp, data=df)
Hpredict <- predict(fit,data.frame(gdp=df$gdp))
lines(df$gdp,Hpredict,col=i)

list <-which(is.na(df$ed_mean))

#y.Missing <-  predict(y.loess, data.frame(x=MissingList))
points(df$gdp[list],Hpredict[list],col="red")



df$dummy <- missDummy(df$ed_mean)

for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$ed_mean[i] = Hpredict[i]
  }
}


write.csv(df,"impute_progress.csv")

library(impute)
#use unimputed data to run knn
nearest <- impute::impute.knn(as.matrix(check))#here k=10
trans <- as.data.frame(nearest$data)

df$dummy <- missDummy(df$meat)
list<-which(df$dummy == 0)

for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$meat[i] = trans$meat[i]
  }
}
which(is.na(df$meat))

plot(df$gdp, df$meat,main="meat by gdp")
mtext("Using knn(k=10)")
points(df$gdp[list],df$meat[list],col="red")

df$dummy <- missDummy(df$air_pollution)
list<-which(df$dummy == 0)

for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$air_pollution[i] = trans$air_pollution[i]
  }
}
which(is.na(df$air_pollution))

plot(df$gdp, df$air_pollution,main="air pollution by gdp")
mtext("Using knn(k=10)")
points(df$gdp[list],df$air_pollution[list],col="red")

df$dummy <- missDummy(df$veggie)
list<-which(df$dummy == 0)

for(i in 1:nrow(df))
{
  if(df$dummy[i] == 0)
  {
    df$veggie[i] = trans$veggie[i]
  }
}
which(is.na(df$veggie))

plot(df$gdp, df$veggie,main="air pollution by gdp")
mtext("Using knn(k=10)")
points(df$gdp[list],df$veggie[list],col="red")

imp <- df[, -c(11,12,14)]
ggpairs(imp,upper = list(continuous = "points"),lower = list(continuous = "cor"))


write.csv(df,"impute_progress.csv")





