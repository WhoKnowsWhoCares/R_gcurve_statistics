library("memisc")
library("lmtest")
library("ggplot2")
library("dplyr")
library("foreign")
library("vcd")
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")
library(reshape)
library(psych)
library(tidyr)
library(plotly)
library(animation)
setwd("~/GitHub/MyProjects/RProjects/GCurveStatistics")
source("clean.R")

one_year <- df$years_01
two_years <- df$years_02
five_years <- df$years_05
fifteen_years <- df$years_15
thirty_years <- df$years_30

st_1 <- shapiro.test(df$years_01)
st_2 <- shapiro.test(df$years_02)
st_5 <- shapiro.test(df$years_05)
st_15 <- shapiro.test(df$years_15)
st_30 <- shapiro.test(df$years_30)

date <- df$Date
df1 <- melt(df ,  id.vars = 'Date', variable.name = 'series')
df1_few <- subset(df,select=c(1,2,3,6,16,31))
df1_few <- melt(df1_few ,  id.vars = 'Date', variable.name = 'series')

df_trans <- as.data.frame(t(df[-1]))
colnames(df_trans) <- as.character(df$Date)
rownames(df_trans) <- sprintf("years_%02d", c(1:30))
df_trans$years <- c(1:30)

full_period <- names(df_trans)
by_year <- full_period[seq(1,length(full_period),by=250)]
df_trans_year <- subset(df_trans, select=by_year)
df_trans_year$years <- c(1:30)
df_trans1_year <- melt(df_trans_year,  id.vars = 'years', variable.name = 'series')

period2008 <- subset(full_period, full_period > as.Date("2008-09-01") & 
                   full_period < as.Date("2009-10-01"))
by_period2008 <- period2008[seq(1,length(period2008),by=20)] 
df_trans_few2008 <- subset(df_trans, select=by_period2008)
df_trans_few2008$years <- c(1:30)
df_trans1_few2008 <- melt(df_trans_few2008 ,  id.vars = 'years', variable.name = 'series')

period20014 <- subset(full_period, full_period > as.Date("2014-04-01") & 
                       full_period < as.Date("2016-06-03"))
by_period2014 <- period20014[seq(1,length(period20014),by=20)] 
df_trans_few2014 <- subset(df_trans, select=by_period2014)
df_trans_few2014$years <- c(1:30)
df_trans1_few2014 <- melt(df_trans_few2014 ,  id.vars = 'years', variable.name = 'series')

kt <- kruskal.test(value ~ variable, data = df1)

lambda <- 0.597761
beta_2_koeff <- function(x){
  return ((1-exp(-lambda*x))/(lambda*x))
}
beta_3_koeff <- function(x){
  return ((1-exp(-lambda*x))/(lambda*x) - exp(-lambda*x))
}
df_trans$beta_2koeff <- beta_2_koeff(df_trans$years)
df_trans$beta_3koeff <- beta_3_koeff(df_trans$years)
beta_hat <- data.frame()
for (i in full_period){
  j <- which(colnames(df_trans)==i)
  model <- lm(data = df_trans, df_trans[[j]]~beta_2koeff+beta_3koeff)
  beta_hat <- rbind(beta_hat,coef(model))
}
colnames(beta_hat) <- c("beta_1","beta_2","beta_3")
beta_hat$Date <- full_period
beta_hat1 <- melt(beta_hat ,  id.vars = 'Date', variable.name = 'series')

model1 <- lm(data = df_trans, df_trans$`2008-09-01`~beta_2koeff+beta_3koeff)
model2 <- lm(data = df_trans, df_trans$`2009-05-04`~beta_2koeff+beta_3koeff)
model3 <- lm(data = df_trans, df_trans$`2009-09-01`~beta_2koeff+beta_3koeff)
model11 <- lm(data = df_trans, df_trans$`2014-04-01`~beta_2koeff+beta_3koeff)
model12 <- lm(data = df_trans, df_trans$`2015-04-01`~beta_2koeff+beta_3koeff)
model13 <- lm(data = df_trans, df_trans$`2016-04-01`~beta_2koeff+beta_3koeff)