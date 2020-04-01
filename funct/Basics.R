ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


plotM<-function(i){
  df.temp<-df[df$Veld==i,]
  df.temp <- df.temp[df.temp$Waarde < quantile(df.temp$Waarde, 0.95, na.rm = T),]
  plot(df.temp$Waarde,df.temp$Perc,
       xlab=i,ylab="%PV")
  abline(lm(df.temp$Perc~df.temp$Waarde))
  
}

rsq <- function (x, y){ cor(x, y) ^ 2}

