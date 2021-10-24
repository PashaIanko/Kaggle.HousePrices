setwd("C:/Users/79137/Pasha/1. Data Science/Kaggle Competitions/2. House prices/Data")
df <- read.csv("train.csv")
attach(df)

# -----

# General
colnames(df)
dim(df)  # 1460 * 81
summary(df)
View(df)

# -----

# Street - type of road access
table(Street)  # Mostly Paved (Can skip this feature)
plot(table(Street))

# -----

# Mszoning
MSZoning.table <- table(MSZoning)
MSZoning.rel.table <- table(MSZoning) / sum(MSZoning.table)
barplot(MSZoning.rel.table)

# -----

# LotFrontage
# Linear feet of street connected to property
summary(LotFrontage)
hist(LotFrontage[!is.na(LotFrontage)], prob=T)  # OK dist!
IQR(LotFrontage, na.rm=T)
median(LotFrontage, na.rm=T)
mean(LotFrontage, na.rm=T)
quantile(LotFrontage, na.rm=T) # Some outliers

# -----

# LotArea
# Size in square feet
summary(LotArea)
hist(LotArea[!is.na(LotArea)], prob=T, breaks=70)  # Outliers
length(LotArea[!is.na(LotArea)]) / length(LotArea)
quantile(LotArea)

# -----

# Alley
# Type of alley access
Alley.table <- table(Alley)
Alley.table.rel <- Alley.table / sum(Alley.table)
length(Alley[!is.na(Alley)]) / length(Alley)  # 6% valid numbers! Drop it

# -----

# LotShape
LotShape.table <- table(LotShape)
LotShape.table.rel <- LotShape.table / sum(LotShape.table)
plot(LotShape.table)
barplot(LotShape.table.rel * 100)
summary(LotShape)

# -----

# LandContour (?)
summary(LandContour)
length(LandContour[is.na(LandContour)])
LandContour.table <- table(LandContour)
barplot(LandContour.table)

# -----

# Utilities
summary(Utilities)
pie(table(Utilities))
barplot(table(Utilities))
sum(Utilities != 'AllPub')  # Compare with test. We can skip feature
sum((Utilities == 'AllPub'))

# -----

# LotConfig
LotConfig.table.rel <- table(LotConfig) / sum(table(LotConfig))
pie(table(LotConfig))
sum(is.na(LotConfig))
barplot(LotConfig.table.rel * 100)
sum(LotConfig == 'FR3')  # Compare to test, remove FR3

