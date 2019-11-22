rm(list=ls()) #clear all variables before beginning
#ctrl + L clears console

circadian <- read.csv(file="C:/Users/borre/Desktop/backup/Grad School/Coursework/BIOSC 2121 - Biostats/Project/Project_Data2.csv")

circadian$Treatment <- factor(circadian$treatment, 
                              levels = c("Control (Cells Alone)", "TNF (20ng/mL)", "TNF (100ng/mL)", "IL-13 (1ng/mL)",
                                         "IL-13 (10ng/mL)","IL-13 (50ng/mL)", "IL-13 (100ng/mL)", "IL-33 (100ng/mL)", 
                                         "UV (30 mins)", "Hypoxia Control")) 

meanCount <- tapply(circadian$shift, circadian$treatment, mean)
sdevCount <- tapply(circadian$shift, circadian$treatment, sd)
n         <- tapply(circadian$shift, circadian$treatment, length)
data.frame(mean = meanCount, std.dev = sdevCount, n = n)

stripchart(shift ~ treatment, data = circadian, method = "jitter", vertical = TRUE)

seCount <- sdevCount / sqrt(n)
adjustAmount <- 0.2
segments( c(1,2,3,4,5) + adjustAmount, meanCount - seCount, 
          c(1,2,3,4,5) + adjustAmount, meanCount + seCount )
points(meanCount ~ c( c(1,2,3,4,5) + adjustAmount ))

circadianAnova <- lm(shift ~ treatment, data = circadian)
anova(circadianAnova)

circadianAnovaSummary <- summary(circadianAnova)
circadianAnovaSummary$r.squared

kruskal.test(shift ~ treatment, data = circadian)