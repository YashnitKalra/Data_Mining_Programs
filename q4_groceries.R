library(arules)
data("Groceries")

summary(Groceries)
class(Groceries) # transactions

itemFrequencyPlot(Groceries,topN=5)
rules=apriori(Groceries,parameter = list(supp=0.001,conf=0.5))

# inspect(rules)

rules=sort(rules,by="confidence",decreasing = TRUE)
inspect(rules[1:10])

# install.packages("arulesViz",dependencies=TRUE)
library(arulesViz)
# plot(rules[1:10])
plot(rules,jitter=0)

plot(rules[1:20],method="graph",control=list(type="items"))

# parallel coordinates
plot(rules[1:20],method="paracoord",control=list(reorder=TRUE))

plot(rules[1:20], method ="matrix")

plot(rules[1:20], method="grouped")

# support = confidence = 60%
#rules=apriori(Groceries,parameter = list(supp=0.6,conf=0.6))
#rules=sort(rules,by="confidence",decreasing = TRUE)
inspect(rules[1:10])
plot(rules,jitter=0)
plot(rules[1:10],method="graph",control=list(type="items"))
plot(rules[1:10],method="paracoord",control=list(reorder=TRUE))
plot(rules[1:10], method ="matrix")
plot(rules[1:10], method="grouped")