rel_feature_table <- function(feature){
  table <- table(feature)
  rel.table <- table / sum(table)
  return(rel.table)
}

pie_plot <- function(rel_table){
  return(pie(rel_table * 100))
}

barplot_summary <- function(Feature){
  par(mfrow=c(1, 2))
  Feature.table.rel <- rel_feature_table(Feature)
  print(Feature.table.rel)
  barplot(Feature.table.rel, names.arg=c(names(Feature.table.rel)))
  boxplot(SalePrice ~ Feature)
  par(mfrow=c(1, 1))
}

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

# -----

# LandSlope
summary(LandSlope)
LandSlope.table <- table(LandSlope)
LandSlope.table.rel <- LandSlope.table / sum(LandSlope.table)
barplot(LandSlope.table.rel)  # Might remove 'Sev' feature (0,8 %)
barplot(LandSlope.table.rel * 100)

# -----

# Neighborhood
Neighborhood.table <- table(Neighborhood)
pie(Neighborhood.table)
Neighborhood.table.rel <- Neighborhood.table / sum(Neighborhood.table)
barplot(Neighborhood.table.rel * 100)

# -----

# Condition1
Condition1.table.rel <- rel_feature_table(Condition1)
pie_plot(Condition1.table.rel)  # We can simplify this feature to "Normal" & "Normal", check correlation

# ----- 

# Condition 2
Condition2.table.rel <- rel_feature_table(Condition2)                                                
pie_plot(Condition2.table.rel)  # Also can simplify or delete, 98 % Normal 
summary(Condition2)
Condition2.table.rel

# -----

# BldgType - type of dwelling
BldgType.table.rel <- rel_feature_table(BldgType)
BldgType.table.rel
boxplot(SalePrice ~ BldgType)
pie_plot(BldgType.table.rel)

# -----

# HouseStyle
boxplot(SalePrice ~ HouseStyle)
HouseStyle.table.rel <- rel_feature_table(HouseStyle)
pie(HouseStyle.table.rel)

# -----

# OverallQual
hist(OverallQual)  # Normal dist, Scale it
boxplot(SalePrice ~ OverallQual)  # 100 % key feature

# -----

# YearBuild
hist(YearBuilt)
plot(SalePrice ~ YearBuilt)
boxplot(SalePrice ~ YearBuilt)  # Also key feature, slight positive trend


condition <- OverallQual > 5
boxplot(SalePrice[condition] ~ OverallQual[condition])

condition <- OverallQual < 5
boxplot(SalePrice[condition] ~ OverallQual[condition])

# -----

# YearRemod - remodelling
plot(SalePrice ~ YearRemodAdd)  # good feature, slight positive trend
plot(OverallQual ~ YearRemodAdd)  # not connected
hist(YearRemodAdd, xlab="year of remodelling", main='Year of remodel', col=629) 
# Can split variable into 3 periods - before 1960, 1960-1990, after 1990

# -----

# Roof style
RoofStyle.table.rel <- rel_feature_table(RoofStyle)
RoofStyle.table.rel
pie_plot(RoofStyle.table.rel)
sum(RoofStyle == 'Mansard')  
boxplot(SalePrice ~ RoofStyle)  # RoofStyle does not really discriminate -> Split to Gable, Hip & Rest

# -----

# Roof Material 
barplot_summary(RoofMatl)  # Split into CompShg and rest of the features

# -----

# Exterior
barplot_summary(Exterior1st)
barplot_summary(Exterior2nd)

# -----

# VnrType
barplot_summary(MasVnrType)

# -----

# Veneer area
par(mfrow=c(1, 2))  # We could do the feature "Has veneer" instead of area
plot(SalePrice ~ MasVnrArea)
summary(MasVnrArea)
length(MasVnrArea[MasVnrArea == 0.0]) / length(MasVnrArea)
hist(MasVnrArea)
par(mfrow=c(1, 1))

# -----

# Quality of material of exterior
barplot_summary(ExterQual)  # Good descriptive feature

# -----

# Condition of exterior material
barplot_summary(ExterCond)  # Ex, Fa, Po merge into one feature

# -----

# Typle of foundation
barplot_summary(Foundation)

# -----

# Basement height
barplot_summary(BsmtQual)  # Descriptive feature


