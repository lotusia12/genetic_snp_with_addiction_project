
library(nlme)

use <- read.csv("imputed_smok_prev.csv")

use$yearF <- 1:5
fit.arh1<-gls(smok_prev~gdp+fruit+veggie+meat+temp+Happiness+air_pollution+ed_mean,data=use,correlation=corAR1(form=~yearF|country),na.action = na.exclude)
df <- data.frame(matrix(ncol = 3, nrow = length(fit.arh1$residuals)))
colnames(df)<- c("country","year","residual")
df$residual=fit.arh1$residuals
df$country=use$country
df$year=use$year


write.csv(df,"smoke_prev_resid.csv")

use <- read.csv("imputed_smok_cons.csv")

use$yearF <- 1:6
fit.arh1<-gls(smok_consum~gdp+fruit+veggie+meat+temp+Happiness+air_pollution+ed_mean,data=use,correlation=corAR1(form=~yearF|country),na.action = na.exclude)
df <- data.frame(matrix(ncol = 3, nrow = length(fit.arh1$residuals)))
colnames(df)<- c("country","year","residual")
df$residual=fit.arh1$residuals
df$country=use$country
df$year=use$year


write.csv(df,"smoke_cons_resid.csv")

use <- read.csv("imputed_alco_prev.csv")

use$yearF <- 1:6
fit.arh1<-gls(alcohol_prev~gdp+fruit+veggie+meat+temp+Happiness+air_pollution+ed_mean,data=use,correlation=corAR1(form=~yearF|country),na.action = na.exclude)
df <- data.frame(matrix(ncol = 3, nrow = length(fit.arh1$residuals)))
colnames(df)<- c("country","year","residual")
df$residual=fit.arh1$residuals
df$country=use$country
df$year=use$year

write.csv(df,"alco_prev_resid.csv")


home <- read.csv("full_Imp_sim.csv")


home$yearF <- 1:8
fit.arh1<-gls(alcohol_consum~gdp+fruit+veggie+meat+temp+Happiness+air_pollution+ed_mean,data=home,correlation=corAR1(form=~yearF|country),na.action = na.exclude)
df <- data.frame(matrix(ncol = 3, nrow = length(fit.arh1$residuals)+1))
colnames(df)<- c("country","year","residual")
df$residual[2:144]=fit.arh1$residuals
df$country=home$country
df$year=home$year


write.csv(df,"alco_cons_resid.csv")
