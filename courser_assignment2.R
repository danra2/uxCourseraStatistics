prefsDevices = read.csv("deviceprefs.csv")
View(prefsDevices)
prefsDevices$Subject = factor(prefsDevices$Subject)
summary(prefsDevices)

# Pearson chi-square test
prfs = xtabs( ~ Pref, data=prefsDevices)
prfs # show counts
chisq.test(prfs)
# Asyomtotic Test, the more values you get the more accurate you get.

# based on variable values
newdata <- prefsDevices[ which(prefsDevices$Disability==0), ]
View(newdata)


# For people without disabilities, perform a binomial test to see whether their preference for touchpads differed significantly from chance. To the nearest ten-thousandth (four digits), what is the p-value? Hint: Run a binomial test comparing the sum of rows of people without disabilities who prefer the touchpad, against the number of all rows of people without disabilities. With two possible preferences, touchpad and trackball, the chance probability would be 1/2. Do not correct for multiple comparisons; consider this a single test on a subset of the data.
plot(prefsDevices[prefsDevices$Disability == "0",]$Pref)
binom.test(sum(prefsDevices[prefsDevices$Disability == "0",]$Pref == "touchpad"), 
           nrow(prefsDevices[prefsDevices$Disability == "0",]), p=1/2)
plot(prefsDevices[prefsDevices$Disability == "0",]$Pref)

plot(prefsDevices[prefsDevices$Disability == "1",]$Pref)
binom.test(sum(prefsDevices[prefsDevices$Disability == "1",]$Pref == "touchpad"), 
           nrow(prefsDevices[prefsDevices$Disability == "1",]), p=1/2)
plot(prefsDevices[prefsDevices$Disability == "1",]$Pref)

m = xtabs(~ Pref + Disability, data=prefsDevices)
chisq.test(m)

m = xtabs(~ Pref + Disability, data=prefsDevices)
library(RVAideMemoire)
G.test(m)

m = xtabs(~ Pref + Disability, data=prefsDevices)
fisher.test(m)
