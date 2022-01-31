longley
data00<-longley
View(data00)
head(data00)
tail(data00)

library(tidyverse)
library(rgl)

### Plotting Employed against each variable
plot(data00$GNP.deflator, data00$Employed)
plot(data00$GNP, data00$Employed)
plot(data00$Unemployed, data00$Employed)
plot(data00$Armed.Forces, data00$Employed)
plot(data00$Population, data00$Employed)
plot(data00$Year, data00$Employed)
#ANS: The Employed is most correlated with GNP, population, Year

###Creating a regression model for employed aginst GNP, population and year
data10<-data00%>%
  select(Employed,GNP,Population,Year)
data10
View(data10)

y<-as.matrix(data10$Employed)
y
View(y)

x<-as.matrix(cbind(rep(1,16), data10[,2:4]))
colnames(x)<-c('x0','x1','x2','x3')
View(x)

#Estimating model equation
B<-solve(t(x)%*%x)%*%t(x)%*%y
B
#ANS: The best model here is GNP

###Recalculate the regression parameters and predicted values from the model matrices
pred<-data10%>%
  mutate(predictedEmployed=416.946+0.068*GNP-0.360*Population-0.172*Year)
pred
View(pred)

#Plot it
plot(pred$Employed, pred$predictedEmployed)

plot3d(x=pred$GNP, y=pred$Population, z=pred$Employed, type = 's', col = 'red')
plot3d(x=pred$GNP, y=pred$Population, z=pred$predictedEmployed, type = 's', col = 'blue',
       add = TRUE)
