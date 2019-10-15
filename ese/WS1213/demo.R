#Daten einlesen
data <- read.csv("C:/.../rt.csv", header=TRUE, sep = ";", dec = ",")

#Minimum/Maximum finden
minValue <- min(data)
maxValue <- max(data)

#Arithmetisches Mittel berechen
meanValue <- mean(data)
medianValue <- median(data)

#Histogramm zeigen (voher ggf. Daten in double konvertieren)
dataNum <- as.numeric(unlist(data))
hist(dataNum)

#Boxplots zeigen
boxplot(data)

#Violinplots; vorher entsprechendes package installieren und laden
install.packages("vioplot")
library(vioplot)

#Violin plot anzeigen
vioplot(dataNum)

#plots in pdf schreiben
#pdf-device öffnen
pdf("C:/.../plots.pdf")
#plot erstellen
boxplot(data)
#pdf-device wieder schließen (kann sonst ggf. nicht geöffnet werden)
dev.off()

#Neuer Datensatz: Antwortzeit von Probanden in zwei Gruppen
input <- read.csv("c:/.../ProgramComprehensionRT.csv", header=TRUE, sep = ";", dec = ".")
dataPC <- input[,3]
#Standardabweichung
sd(dataPC)
hist(dataPC)
boxplot(dataPC)
vioplot(dataPC)

dataPC1 <- input[1:7,3]
dataPC2 <- input[8:13,3]

#Konfidenzintervalle bestimmen
dataPC1.ci = t.test(dataPC1)$conf.int
dataPC2.ci = t.test(dataPC2)$conf.int

#Balkendiagramm mit Konfidenzintervallen
#Bibliothek laden (entsprechende benötigte Pakete werden automatisch mitgeladen)
library(gplots)
barplot2(
    c(mean(dataPC1),mean(dataPC2)),
    ci.l=c(dataPC1.ci[1],dataPC2.ci[1]),
    ci.u=c(dataPC1.ci[2],dataPC2.ci[2]),
    plot.ci=TRUE
)

#T-Test für unabhängige Stichproben
t.test(dataPC1, dataPC2)

#Shapiro-Wilk Test für Normalverteilung
shapiro.test(dataPC)

#Mann-Whitney-U test (unabhänigige Stichproben)
wilcox.test(dataPC1,dataPC2,,paired=FALSE)































