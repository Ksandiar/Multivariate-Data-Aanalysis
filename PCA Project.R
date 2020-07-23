# Importing data
labor = read.table("labor21_23.dat",header=T)
library(plyr)
labor = rename(labor,c('h212311'= 'Groceries','h212312'= 'Meals out','h212313a'='Education',
               'h212315'= 'Vehicle', 'h212316' = 'Housing','h212318'= 'Medical',
               'h212319' ='Recreation', 'h212321' = 'Communications',
               'h212326'= 'Clothing', 'h212332'= 'Health insurance',
               'h212329'='Public transportation', 'h212102'= 'Earned income',
               'h212402'= 'Monthly savings' ))
PCA_data = labor[,c('Groceries','Meals out','Education','Vehicle','Housing','Medical',
                 'Recreation','Communications','Clothing','Health insurance',
                 'Public transportation')]
#Exploring data
str(labor)

summary(labor)

par(mfrow = c(2, 2))
for (i in 1:11){
  qqnorm(PCA_data[,i],main = colnames(PCA_data)[i],cex.main=2.4)
}

shapiro.test(PCA_data$Groceries)

#Conducting PCA
expense.pca = prcomp(PCA_data,center=T,scale=T)
expense.pca
summary(expense.pca)
expense.pca$sdev^2
screeplot(expense.pca, type="l", main='Scree Plot for expense.pca', cex.main = 2.4)


library(scatterplot3d)

PC_scores = data.frame(expense.pca$x)
PC1 = PC_scores[,1]
PC2 = PC_scores[,2]
PC3 = PC_scores[,3]
par(mfrow = c(1,2))
scatterplot3d(PC_scores[,1:3],pch = 16, angle = 60,main = '3D scatterplot of PC1 & PC2 & PC3', cex.main = 2.4)
plot(x=PC1,y=PC2,pch=16,main = '2D scatterplot of PC1 & PC2', cex.main = 2.4,xlab = 'PC1', ylab = 'PC2')
plot(x=PC1,y=PC3,pch=16,main = '2D scatterplot of PC1 & PC3',cex.main=2.4,xlab = 'PC1',ylab = 'PC3')
plot(x=PC2,y=PC3,pch=16,main = '2D scatterplot of PC2 & PC3',cex.main=2.4,xlab = 'PC2',ylab = 'PC3')

cor(PC1,PC2)
cor(PC1,PC3)
cor(PC2,PC3)