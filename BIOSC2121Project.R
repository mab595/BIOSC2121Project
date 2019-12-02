rm(list=ls()) #clear all variables before beginning
#ctrl + L clears console

# Start writing to an output file
sink('analysis-output.txt')

set.seed(12345)
x <-rnorm(10,10,1)
y <-rnorm(10,11,1)

circadian <- read.csv(file="C:/Users/borre/Desktop/backup/Grad School/Coursework/BIOSC 2121 - Biostats/Project/Project_Data2.csv")

circadian$Treatment <- factor(circadian$treatment, 
                              levels = c("Control (Cells Alone)", "TNF (20ng/mL)", "TNF (100ng/mL)", "IL-13 (1ng/mL)",
                                         "IL-13 (10ng/mL)","IL-13 (50ng/mL)", "IL-13 (100ng/mL)", "IL-33 (100ng/mL)", 
                                         "UV (30 mins)", "Hypoxia Control")) 

meanCount <- tapply(circadian$count, circadian$treatment, mean)
sdevCount <- tapply(circadian$count, circadian$treatment, sd)
n         <- tapply(circadian$count, circadian$treatment, length)
data.frame(mean = meanCount, std.dev = sdevCount, n = n)

png("ProjChart.png",width = 720, height=480)

stripchart(count ~ treatment, data = circadian, method = "jitter", vertical = TRUE)

seCount <- sdevCount / sqrt(n)
adjustAmount <- 0.2
segments( c(1,2,3,4,5) + adjustAmount, meanCount - seCount, 
          c(1,2,3,4,5) + adjustAmount, meanCount + seCount )
points(meanCount ~ c( c(1,2,3,4,5) + adjustAmount ))

dev.off()

circadianAnova <- lm(count ~ treatment, data = circadian)
anova(circadianAnova)

circadianAnovaSummary <- summary(circadianAnova)
circadianAnovaSummary$r.squared

#kruskal.test(count ~ treatment, data = circadian)

P_adj=p.adjust(.05, "bonferroni",n=10)

pairwise.t.test(circadian$count,circadian$treatment, p.adj = "bonferroni")
pairwise.t.test(circadian$count,circadian$treatment, p.adj="holm")



# Stop writing to the file
sink()


# Append to the file
sink('analysis-output.txt', append=TRUE)
cat("Some more stuff here...\n")
sink()
