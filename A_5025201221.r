install.packages("BSDA")
library(BSDA)
# no 1

# a

before = c(78, 75, 67, 77, 70, 72, 78, 74, 77)
after = c(100, 95, 70, 90, 90, 90, 89, 90, 100)
sd(after-before)

# b

mean = mean(after-before)
2 * pt(-abs(mean/(sd(after-before)/sqrt(9))), df = 8)

# c

t.test(x=after, y=before, alternative = "two.sided", mu = 0)

# no 2

zsum.test(mean.x=23500, sigma.x = 3900, n.x = 100,  
          alternative = "greater", mu = 200000, conf.level = 0.95)

# no 3

# b

((19-1)*1.67*1.67 + (27-1)*1.32*1.32)/ ((19-1)+(27-1))

#c

(3.64-2.79 - 0)/(sqrt(((19-1)*1.67*1.67 + (27-1)*1.32*1.32)/ ((19-1)+(27-1))*(1/19 + 1/27)))
qt(p=0.95, df=2)

upper = (3.64-2.79) + qt(p=0.95, df=2)*(sqrt(((19-1)*1.67*1.67 + (27-1)*1.32*1.32)/ ((19-1)+(27-1))*(1/19 + 1/27)))
lower = (3.64-2.79) - qt(p=0.95, df=2)*(sqrt(((19-1)*1.67*1.67 + (27-1)*1.32*1.32)/ ((19-1)+(27-1))*(1/19 + 1/27)))
upper
lower

# no 4

# a

Dataset = read.table("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt")

temp = subset(Dataset, V1 == "1")
Grup1 <- as.numeric((temp$V2))
Grup1
temp = subset(Dataset, V1 == "2")
Grup2 <- as.numeric((temp$V2))
Grup2
temp = subset(Dataset, V1 == "3")
Grup3 <- as.numeric((temp$V2))
Grup3

qqnorm(Grup1)
qqline(Grup1)

qqnorm(Grup2)
qqline(Grup2)

qqnorm(Grup3)
qqline(Grup3)

# b

bartlett.test(list(Grup1, Grup2, Grup3))

# no 5