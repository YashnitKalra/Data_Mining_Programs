?rivers
rivers

summary(rivers)
hist(rivers)
?boxplot
boxplot(rivers,horizontal = TRUE)
riv1=rivers[rivers<1250]
boxplot(riv1, horizontal = T)
riv2=rivers[rivers<1050]
boxplot(riv2, horizontal = T)
hist(riv2)
