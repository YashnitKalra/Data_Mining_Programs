df=data.frame(a=c(1,2,3,4,-5),b=c(-1,-2,-3,-4,-5))
df
summary(df)
mean(df$a)
sd(df$a)
# install.packages("caret")
library(caret)
df1=df
pre_df1=preProcess(df1,method=c("center"))
# "center" -> mean, "scale" -> sd
pre_df1

mead_df1=predict(pre_df1,df1) # subtracted mean
mead_df1
summary(mead_df1)

pre_df2=preProcess(df1,method=c("scale"))
pre_df2

sd_df1=predict(pre_df2,df1)
sd_df1
summary(sd_df1)
sd(sd_df1$a)
