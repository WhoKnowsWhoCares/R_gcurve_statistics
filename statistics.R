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

smth <- InsectSprays
?InsectSprays
summary(df)

X1 <- select(df,years_01)
ggplot(df,aes(X1))+
  geom_histogram(fill="white", col="black", binwidth=0.4)
ggplot(df,aes(X30))+
  geom_histogram(fill="white", col="black", binwidth=0.4)

summary(df[c(2,6,11)])

one_year <- df$years_01
two_years <- df$years_02
five_years <- df$years_05
thirty_years <- df$years_30
summary(cbind(one_year, two_years))
#normality
st <- shapiro.test(df$years_01)
st
#???
wt <- wilcox.test(df$years_01)
wt
#???
mb <- mean_cl_boot(df$years_01)
mb

#3d plot by plotly
z <- matrix(df)
z <- z[c(2:length(z[,1])),]
a <- list(
  title = "дата")
b <- list(
  title = "срок до погашения")
c <- list(
  title = "доходность")
scene = list(
  xaxis = a,
  yaxis = b,
  zaxis = c)
plot_ly(data=df, x = Date, z = z, type = "surface") %>%
  layout(title="3D структура процентных ставок", scene=scene)

#normality
ggplot(df1_few,aes(value))+
  geom_histogram(fill="white", col="black", binwidth=0.2)+
  facet_wrap(~ variable, ncol = 5)

#time dynamic single period bond
years_30 <- names(df)[31]
plot <- ggplot(data = df, aes(x = Date, y = years_30)) +
  geom_line() +
  coord_cartesian(ylim = c(5, 15)) +
  labs(title=years_30)+
  stat_smooth(method="lm")
plot

#save in file
#ggsave(filename="30.png", plot=plot)
#plot2file(plot,"function_plot2file.png")
#df2file(df,df$Date, names(df)[11], 1,"t_")
df2files(df,df$Date,names(df)[2:length(names(df))],"t_")
system("magick -delay 50 t_*.png output.gif")

#combine data
date <- df$Date
recent_date <- subset(date, date > as.Date("2003-01-01") & 
                        date < as.Date("2016-05-01"))
df_sub <- subset(df,df$Date %in% recent_date)
df1 <- melt(df_sub ,  id.vars = 'Date', variable.name = 'series')

#tests on melted data
M <- lm(value ~ variable, data = df1)
summary(M)
?boxplot
boxplot(value ~ variable,
        xlab = "срок погашения",
        ylab = "доходность",
        main="Распределение процентной ставки",
        names=c(1:30),
        col = "coral", data = df1)

#groups are different statistically
kruskal.test(value ~ variable, data = df1)
#normality test
#shapiro.test(resid(M))

#time dynamic all periods bonds
summary_plot_1 <- ggplot(data = df1, aes(x = Date, y = value)) +
  geom_line(aes(colour = variable))+
  xlab("дата рассчета")+
  ylab("% ставка")+
  labs(colour="")
summary_plot_1
summary_plot <- ggplot(data = df1, aes(x = Date, y = value)) +
  geom_line() + 
  facet_wrap(~ variable)+
  stat_smooth(method="lm")+
  ggtitle("Временные колебания процентных ставок") + 
  xlab("дата рассчета")+
  ylab("% ставка")+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
summary_plot
#ggplot(data = df1, aes(x = Date, y = value)) +
#    geom_line(aes(colour = variable)) +
#    facet_grid(variable ~ .)

#hystograms
ggplot(df1,aes(value))+
  geom_histogram(fill="white", col="black", binwidth=0.4)+
  facet_wrap(~ variable)

#save all
df_trans <- as.data.frame(t(df[-1]))
colnames(df_trans) <- as.character(df$Date)
rownames(df_trans) <- sprintf("years_%02d", c(1:30))
df_trans$years <- c(1:30)

#graph
plot_trans <- ggplot(data = df_trans, aes(x = years, y = df_trans$`2003-01-05`)) +
  geom_line() + 
  ggtitle(paste0("Кривая доходности")) + 
  xlab("срок погашения")+
  ylab("доходность")
plot_trans

#save in files by data
full_period <- names(df_trans)
short_period <- subset(full_period, full_period > as.Date("2008-09-01") & 
                         full_period < as.Date("2009-06-01"))
#short_period <- subset(full_period, full_period > as.Date("2014-04-01") & 
#                         full_period < as.Date("2016-05-01"))
#ggsave(filename="2file.png", plot=plot_trans)
#plot2file(plot_trans,"function_plot2file.png")
#df2file(df_trans,df_trans$years,as.Date("2003-01-05"),"date_")
df2files(df_trans,df_trans$years, short_period,"date_")
system("magick -delay 10 date_*.png date_output2014.gif")

#all data
df_trans1 <- melt(df_trans, id.vars = 'years', variable.name = 'series')
ggplot(data = df_trans1, aes(x = years, y = value)) +
  geom_point()

#partial
#start <- as.Date("2014-04-01")
#end <- as.Date("2016-05-01")
#period <- seq(start,end,by="week")
#contain_dates <- subset(short_period, period %in% short_period)
period <- subset(full_period, full_period > as.Date("2014-04-01") & 
                   full_period < as.Date("2016-06-03"))
by_year <- full_period[seq(1,length(full_period),by=250)]
by_period <- period[seq(1,length(period),by=20)] 
df_trans_few <- subset(df_trans, select=by_period)
df_trans_few$years <- c(1:30)
df_trans1_few <- melt(df_trans_few ,  id.vars = 'years', variable.name = 'series')
ggplot(data = df_trans1_few, aes(x = years, y = value)) +
  geom_line(aes(colour = variable))+
  ggtitle("Кривые доходностеи с 2014 по 2016 с периодом в месяц")+
  xlab("срок погашения")+
  ylab("доходность")+
  labs(colour="")
  #theme(legend.position="none")

#MHK
df_trans_year$beta_2_koeff <- beta_2_koeff(df_trans_year$years)
df_trans_year$beta_3_koeff <- beta_3_koeff(df_trans_year$years)
beta_hat <- data.frame()
for (i in full_period){
  j <- which( colnames(df_trans)==i )
  model <- lm(data = df_trans, df_trans[[j]]~beta_2_koeff+beta_3_koeff)
  beta_hat <- rbind(beta_hat,coef(model))
}
colnames(beta_hat) <- c("beta_1","beta_2","beta_3")
beta_hat$Date <- full_period
#colnames(beta_hat) <- c("coeff",by_period)
beta_hat1 <- melt(beta_hat ,  id.vars = 'Date', variable.name = 'series')
ggplot(data = beta_hat1, aes(x=Date,y=value)) +
  geom_point(aes(colour=variable), size=1)+
  ggtitle("Коэффициенты модели Нельсона-Сигеля")+
  xlab("дата")+
  ylab("значение")+
  labs(colour="")+
  coord_cartesian(ylim = c(-10, 15))+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
summary(model)
