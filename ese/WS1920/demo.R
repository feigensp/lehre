#Read file
input <- read.csv("C:/../rt.csv", sep=";", dec=",")
rt <- input[,'time..task']

#Minimum/Maximum
minValue <- min(rt)
maxValue <- max(rt)

#Arithmetic mean
meanValue <- mean(rt)
medianValue <- median(rt)

#Histogram
rtNum <- as.numeric(unlist(rt))
hist(rtNum)

#Boxplots
boxplot(rtNum)

#Violinplots; install and load package
install.packages("vioplot")
library(vioplot)

#Violin plot anzeigen
vioplot(rtNum)


#write plots to pdf
#open pdf-device
pdf("plots.pdf")
#create plot
boxplot(data)
#close pdf-device (file might not be readable otherwise)
dev.off()

#Select all rows, in which the programming language is Haskell; from this, select column with the resonse time
rt1 <- subset(rt,pl=='haskell')[,'time..task']
rt2 <- subset(rt,pl=='Java')[,'time..task']

#T-Test for independent samples
t.test(rt1, rt2)

#Shapiro-Wilk Test for normal distribution
shapiro.test(rt)

#Mann-Whitney-U test (independent samples)
wilcox.test(rt1,rt2,paired=FALSE)

#correlation
rtTask2 <- input[,'time2']
plot(rt,rtTask2)
cor.test(rt,rtTask2, method="pearson")
cor.test(rt,rtTask2, method="spearman")

#more at: http://rtutorialseries.blogspot.de/
