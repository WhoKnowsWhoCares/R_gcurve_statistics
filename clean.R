library(tidyr)
setwd("~/GitHub/MyProjects/RProjects/GCurveStatistics")

df <- read.csv("GCurve_full.csv",col.names = c("Date",sprintf("years_%02d", c(1:30))),sep = ";",dec = ",")
df$Date <- as.Date(df$Date,format="%d/%m/%Y")

#names <- names(df)
#names <- names[c(2:length(names))]

plot2file <- function(theplot, name, ...) {
  name <- paste0(name, ".png")
  png(filename=name)
  print(theplot)
  dev.off()
}

df2file <- function(df, t, y, counter, prefix) {
  i <- which( colnames(df)==y )
  percent <- df[[i]]
  years <- t
  plot <- ggplot(data = df, aes(x = years, y = percent)) +
    geom_line() + 
    coord_cartesian(ylim = c(6, 14)) +
    labs(title=y)
    #stat_smooth(method="lm")
  plot2file(plot, paste0(prefix,counter))
}

df2files <- function(df, x ,v, prefix){
  numbers <- sprintf("%03d", c(1:length(v)))
  counter <- 1
  for (i in v){
    df2file(df, x, i,numbers[counter],prefix)
    counter <- counter + 1
  }
}