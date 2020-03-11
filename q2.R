# "http://raw.github.com/edwindj/datacleaning/master/data/dirty_iris.csv"

# which(is.na()) : gives position where 'na' is present in vector
# is.na(), is.nan(), is.infite(), is.finite()

# demo
dat=data.frame(a=c(1,Inf),b=c(Inf,3),d=c("a","b"))
dat
is.na(dat)<-sapply(dat,is.infinite)
dat

# dirty iris
# i)
df=read.csv("dirty_iris.csv")
head(df)
c=sum(complete.cases(df))
percentage_complete=c/nrow(df)*100
paste("Complete Cases: ",percentage_complete,"%",sep="")

# ii)
df[86,]
is.na(df)=sapply(df,is.infinite)
df[86,]

# iii)
library("editrules")
E=editfile("q2_rules.txt")
E
summary(E)

# iv)
cleanedData=violatedEdits(E,df)
View(cleanedData)
summary(cleanedData)
pie(table(cleanedData))

# v)
boxplot(df$Sepal.Length,horizontal = T)
boxplot.stats(df$Sepal.Length)
df2=df$Sepal.Length[df$Sepal.Length>0 & df$Sepal.Length<49]
boxplot(df2,horizontal = T) # notch = T for conf in stats.
