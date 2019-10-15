#Daten einlesen
data <- read.csv("C:/Users/jesse/Desktop/vl/data/Lehrmethode.csv", header=TRUE, sep = ";", dec = ",")

UV <- data[,1]
AV <- data[,2]

#One-way ANOVA
summary(aov(AV~UV))

#Pairwise comparisions
TukeyHSD(aov(AV~UV))

#2-way ANOVA
#Daten einlesen
data <- read.csv("C:/Users/jesse/Desktop/vl/data/treatments.csv", header=TRUE, sep = ";", dec = ",")

A <- data[,1]
B <- data[,2]
AV <- data[,3]

summary(aov(AV~A*B))

#Interaktions-Diagramm
interaction.plot(A,B, AV)

#Einfaktorielle Varianzanalyse mit Messwiederholung
data <- read.csv("C:/Users/jesse/Desktop/vl/data/oneWayRepeated.csv", header=TRUE, sep = ",", dec = ".")

#Preparing the data for R
ageLevels <- c(1, 2, 3)
ageFactor <- as.factor(ageLevels)
ageFrame <- data.frame(ageFactor)
ageBind <- cbind(data$Interest10, data$Interest15, data$Interest20)

#Lineares Modell
ageModel <- lm(ageBind ~ 1)
summary(Anova(ageModel, idata = ageFrame, idesign = ~ageFactor))

#Pairwise comparisions
#Rearrange data first to format of one-way ANOVA
TukeyHSD(aov(AV~UV))

#Zweifaktorielle ANOVA mit Messwiederholung
data <- read.csv("C:/Users/jesse/Desktop/vl/data/twoWayRepeated.csv", header=TRUE, sep = ",", dec = ".")
idata <- read.csv("C:/Users/jesse/Desktop/vl/data/twoWayRepeatedidata.csv", header=TRUE, sep = ",", dec = ".")

#Prepare the data set
interestBind <- cbind(data$schoolAge10, data$schoolAge15, data$schoolAge20, data$workAge10, data$workAge15, data$workAge20)

#Define the model
interestModel <- lm(interestBind ~ 1)
analysis <- Anova(interestModel, idata = idata, idesign = ~Interest * Age)