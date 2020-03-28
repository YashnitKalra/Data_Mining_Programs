library(arules)
df=read.csv("apriori/1000i.csv",header=FALSE)
colnames(df)=c("Receipt_Number","Food","Quantity")
head(df)

l=1:5
items=c("Milk","Sugar","Choco","Apples","Curd")
df1=data.frame(l,items)

df$Food = df1$items[match(df$Food, df1$l)]
head(df)

df_t=as(split(df$Food,df$Receipt_Number),"transactions")
summary(df_t)

rules=apriori(df_t,parameter=list(supp=0.001, conf=0.001))
rules=sort(rules,by="confidence",decreasing = TRUE)
inspect(rules)

plot(rules)

itemFrequencyPlot(df_t)