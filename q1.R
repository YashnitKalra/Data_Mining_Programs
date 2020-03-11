# editrules
# editset => rules same files
# editfile => seperate file
# E <- editset(expression(Age>=0,...)), %in% c('signle','...',...) (values belong to one of these)
# if(Age<18) agegroup=='child'

# install.packages("editrules")
library("editrules")
df=read.table("people.txt",header = TRUE)
df
E=editset(expression(
  Age>=0,
  Age<=150,
  Age>yearsmarried,
  status %in% c('single','married','widowed'),
  if(Age<18) agegroup=='child',
  if(Age>=18 && Age<=65) agegroup=='adult',
  if(Age>65) agegroup=='elderly'
))
E
summary(E)
cleanedData=violatedEdits(E,df)
cleanedData
View(cleanedData) # TRUE means there is violation
summary(cleanedData)
barplot(table(cleanedData))
