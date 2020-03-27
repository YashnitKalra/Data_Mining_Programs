wine_df=read.csv("https://gist.githubusercontent.com/tijptjik/9408623/raw/b237fa5848349a14a14e5d4107dc7897c21951f5/wine.csv")
head(wine_df)

# install.packages("caret",dependencies = TRUE)
library(caret)

# wine
wine_df1=wine_df

pre_df1=preProcess(wine_df1,method=c("center","scale"))
df2=predict(pre_df1,wine_df1)

head(df2)
summary(df2)
sd(df2$Alcohol)

# iris

iris_df=iris

pre_df2=preProcess(iris_df,method=c("center","scale"))
df3=predict(pre_df2,iris_df)

head(df3)
summary(df3)
sd(df3$Sepal.Length)