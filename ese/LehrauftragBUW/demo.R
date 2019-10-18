#Read file
input <- read.csv("G:/work/rt.csv", sep=";", dec=",")
rt <- input[,'time']

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

#Select all rows, in which group is 1; from this, select column time
rt1 <- subset(input,group==1)[,'time']
rt2 <- subset(input,group==2)[,'time']

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
