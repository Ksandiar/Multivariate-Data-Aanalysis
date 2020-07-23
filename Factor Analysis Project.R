#Importing Data
survey = read.table('survey_23.dat',header = T, sep='\t')
survey
#Recode Data
Positive = c(1,3,5,6,7,11,12,15,17,21,22,25) #Positive Questions
Negative = setdiff(1:25, c(1,3,5,6,7,11,12,15,17,21,22,25)) #Negative Qusetions

for (i in Positive){
  survey[,i+3][survey[,i+3] == 'Very like'] = 3
  survey[,i+3][survey[,i+3] == 'Moderately like'] = 2
  survey[,i+3][survey[,i+3] == 'Moderately unlike'] = 1
  survey[,i+3][survey[,i+3] == 'Very unlike'] = 0
}

for (j in Negative){
  survey[,j+3][survey[,j+3] == 'Very like'] = 0
  survey[,j+3][survey[,j+3] == 'Moderately like'] = 1
  survey[,j+3][survey[,j+3] == 'Moderately unlike'] = 2
  survey[,j+3][survey[,j+3] == 'Very unlike'] = 3
}

for (k in (1:25)){
  survey[,k+3] = as.integer(survey[,k+3])
}

summary(survey) # two NA in Q17 AND Q23

is.na(survey['Q17']) #obs 8
is.na(survey['Q23']) #obs 30

#impute missing values using correlation

survey_reduced = survey[-c(8,30),]

sum(is.na(survey_reduced))

cor_reduced = cor(survey_reduced[,4:28])
sort(cor_reduced[,17],decreasing = T) # Highest correlation with Q5
sort(cor_reduced[,23],decreasing = T) # Highest correlation with Q9

survey[8,20] = survey[8,8]
survey[30,26] = survey[30, 12]

sum(is.na(survey))

#plotting barplot

for (u in (1:25)){
  survey[,u+3] = factor(survey[,u+3],levels = c(0,1,2,3))
}
par(mfrow = c(2, 2))
barplot(table(survey$Q1), ylab = "Freq",xlab = "Q1", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q2), ylab = "Freq",xlab = "Q2", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q3), ylab = "Freq",xlab = "Q3", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q4), ylab = "Freq",xlab = "Q4", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q5), ylab = "Freq",xlab = "Q5", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q6), ylab = "Freq",xlab = "Q6", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q7), ylab = "Freq",xlab = "Q7", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q8), ylab = "Freq",xlab = "Q8", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q9), ylab = "Freq",xlab = "Q9", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q10), ylab = "Freq",xlab = "Q10", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q11), ylab = "Freq",xlab = "Q11", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q12), ylab = "Freq",xlab = "Q12", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q13), ylab = "Freq",xlab = "Q13", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q14), ylab = "Freq",xlab = "Q14", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q15), ylab = "Freq",xlab = "Q15", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q16), ylab = "Freq",xlab = "Q16", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q17), ylab = "Freq",xlab = "Q17", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q18), ylab = "Freq",xlab = "Q18", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q19), ylab = "Freq",xlab = "Q19", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q20), ylab = "Freq",xlab = "Q20", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q21), ylab = "Freq",xlab = "Q21", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q22), ylab = "Freq",xlab = "Q22", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q23), ylab = "Freq",xlab = "Q23", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
barplot(table(survey$Q24), ylab = "Freq",xlab = "Q24", cex.names = 1.6, cex.axis = 1.6, cex.lab = 1.6)
par(mfrow = c(1, 1))
barplot(table(survey$Q25), ylab = "Freq",xlab = "Q25",cex.names = 2, cex.axis = 2, cex.lab = 2)

for (k in (1:25)){
  survey[,k+3] = as.integer(survey[,k+3])
}

library(psych)
describe(survey)

# Conducting Factor Analysis


cor=cor(survey[,4:28])
cor

survey.pca=princomp(covmat=cor,cor=T)
summary(survey.pca)
survey.pca$sdev^2
screeplot(survey.pca, npcs=25,type="l")
survey.pca$loadings

fa_pa_n3 = fa(cor, nfactors=3, fm="pa", rotate="varimax")
fa_pa_n3

fa_ml_n1 = fa(cor, nfactors=1, n.obs=52, fm="ml", rotate="varimax")
fa_ml_n1

fa_ml_n2 = fa(cor, nfactors=2, n.obs=52, fm="ml", rotate="varimax")
fa_ml_n2

fa_ml_n3 = fa(cor, nfactors=3, n.obs=52, fm="ml", rotate="varimax")
fa_ml_n3

#Dividing byGender
library(dplyr)
survey_m = survey %>%
  filter (Gender == 'Male')
survey_f = survey %>%
  filter (Gender == 'Female') 

cor_m = cor(survey_m[,4:28])
survey_m.pca=princomp(covmat=cor_m,cor=T)
summary(survey_m.pca)
survey_m.pca$sdev^2
screeplot(survey_m.pca, npcs=25,type="l")

fa_pa_n3_m = fa(cor_m, nfactors=3, fm="pa", rotate="varimax")
fa_pa_n3_m

cor_f = cor(survey_f[,4:28])
survey_f.pca=princomp(covmat=cor_f,cor=T)
summary(survey_f.pca)
survey_f.pca$sdev^2
screeplot(survey_f.pca, npcs=25,type="l")

fa_pa_n3_f = fa(cor_f, nfactors=3, fm="pa", rotate="varimax")
fa_pa_n3_f

#Dividing by Guardian
library(dplyr)
survey_father = survey %>%
  filter (Guardian == 'Father')
survey_mother = survey %>%
  filter (Guardian == 'Mother') 

cor_father = cor(survey_father[,4:28])
survey_father.pca=princomp(covmat=cor_father,cor=T)
summary(survey_father.pca)
survey_father.pca$sdev^2
screeplot(survey_father.pca, npcs=25,type="l")

#Error due to lack of obs
fa_pa_n3_father = fa(cor_father, nfactors=3, fm="pa", rotate="varimax")
fa_pa_n3_father

fa_ml_n3_father = fa(cor_father, nfactors=3, n.obs=9, fm="ml", rotate="varimax")
fa_ml_n3_father

cor_mother = cor(survey_mother[,4:28])
survey_mother.pca=princomp(covmat=cor_mother,cor=T)
summary(survey_mother.pca)
survey_mother.pca$sdev^2
screeplot(survey_mother.pca, npcs=25,type="l")

fa_pa_n3_mother = fa(cor_mother, nfactors=3, fm="pa", rotate="varimax")
fa_pa_n3_mother

#FA with 2 factors
fa_pa_n2 = fa(cor, nfactors=2, fm="pa", rotate="varimax")
fa_pa_n2
