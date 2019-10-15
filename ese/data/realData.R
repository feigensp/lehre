#Daten einlesen
data <- read.csv("C:/Users/jesse/Desktop/vl/data/dataTasks.csv", header=TRUE, sep = ";", dec = ",")

#Antwortzeiten für Antworten mit 0, 1 oder 2 Punkten auslesen (ohne vorher zu sortieren)
#die Daten haben danach den Typ double, so dass ein konvertieren nicht mehr nötig ist.
RT1_0 <- subset(data$X1..time,data$X1..Correct==0)
RT1_1 <- subset(data$X1..time,data$X1..Correct==1)
RT1_2 <- subset(data$X1..time,data$X1..Correct==2)

#Typabfrage
typeof(RT1_0)

#Antwortzeiten unabhängig von Korrektheit auswählen
RT1 <- data[1:40,3]
RT2 <- data[1:40,9]

#Histogramm zeigen (voher ggf. Daten in double konvertieren)
RT1 <- as.numeric(unlist(RT1))
hist(RT1)

#Boxplots zeigen
boxplot(RT1,RT1_0,RT1_1,RT1_2)

#Violinplots; vorher entsprechendes package installieren und laden
install.packages("vioplot")
library(vioplot)

#Violin plot anzeigen
vioplot(RT1,RT1_0,RT1_1,RT1_2)

#Shapiro-Wilk Test für Normalverteilung
shapiro.test(RT1)

#Mann-Whitney-U test (unabhänigige Stichproben)
wilcox.test(RT1,RT2,paired=TRUE)

#chi^2-Test
#matrix mit häufigkeiten definieren
freqsGender <- matrix(c(9,3),nrow=1)
freqs <- matrix(c(6,3,18,15,16,22),nrow=2)
chisq.test(freqs)

#Erwartete Häufigkeitsverteilung mit angeben beim Durchführen des Tests)
chisq.test(freqsGender,p=c(0.7,0.3))

#fisher's exact test
fisher.test(freqs)

#scatterplot
plot(RT1,RT2)

#Pearson Correlation
cor(RT1, RT2, method="pearson")

#Spearman Correlation
Diff1 <- data[1:40,4]
Diff2 <- data[1:40,10]
cor(Diff1, Diff2, method="spearman")

#Regression
regression <- lm(RT2~RT1)































