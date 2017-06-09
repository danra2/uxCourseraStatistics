pgviews = read.csv("timeonsite.csv")
View(pgviews)
pgviews$Subject = factor(pgviews$Subject) # convert to nominal factor
summary(pgviews)

# 3
library(plyr)
ddply(pgviews, ~ Site, function(data) summary(data$Time))

#4
ddply(pgviews, ~ Site, summarise, Time.mean=mean(Time), Time.sd=sd(Time))

#5
t.test(Time ~ Site, data=pgviews, var.equal=TRUE)

#7
t.test(Time ~ Site, data=pgviews, var.equal=TRUE)

t.test(Time ~ Site, data=pgviews, var.equal=TRUE)