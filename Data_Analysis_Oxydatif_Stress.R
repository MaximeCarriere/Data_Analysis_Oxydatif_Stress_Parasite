#############################################################################
#######                       DATA ANALYSIS                          ########
#######                 IMPACT OF PARASITE IN                        ########
#######                     OXYDATIF STRESS                          ########
#############################################################################



data(Appro)


#############################################################################
#######                      PART A                                  ########
#############################################################################


library(corrplot)
M=Appro[, 5:10] 
M
summary(M)
M1 =na.omit(M)
M1=na.omit(M)
M1
cor(M1)
T=cor(M1[,1:6])
corrplot(T)
corrplot(M1, type="upper", order="hclust", row.names(1))
 
T

P=na.omit(Appro2)
P
H=P[5:8]
H
cor(H)
corrplot(P)

cor(M1)
corrplot(M1, method="circle")

S=Appro3[,5:8]
S1=na.omit(S)
S1
corrplot(S1)
summary(S1)
library(corrplot)
M2=na.omit(Appro)
M=cor(Appro[, 5:10] )
corrplot(M2, type="upper", order="hclust", tl.col="black", tl.srt=106)



#############################################################################
#######                      PART B                                  ########
#############################################################################

lm1=lm(ROM ~ Longueure, Appro) 
summary(lm1)

lm2=lm(ROM~Poids, Appro)
summary(lm2)

lm3=lm(ROM~Poids_foie, Appro)
summary(lm3)

lm4 = lm(ROM~Sexe, Appro)
summary(lm4)

lm5 = lm(ROM~phtalate, Appro)
summary(lm5)

lm6=lme(ROM~Parasite, Appro)
summary(lm6)

lm7 = lm(ROM~ site, Appro)
summary(lm7)

lm8= lm(ROM~OXY, Appro)
summary(lm8)

lm9= lm(ROM ~ Longueure + Poids + phtalate + Sexe + Parasite + site + OXY, Appro)
summary(lm9)


glmN = glm(OXY ~ parasite + phtalate + parasite:phtalate, dataa, family=gaussian())

lm10= glm(ROM ~ Gabarit + phtalate + OXY, AP, family=gaussian())
summary(lm10)

lm11= glm(ROM ~ Gabarit + phtalate + OXY + phtalate:Gabarit, AP, family=gaussian() )
summary(lm11)

lm12=glm(ROM ~ Gabarit + OXY, AP, family=gaussian())
summary(lm12)
lm14=glm(ROM ~ Gabarit + phtalate, AP, family=gaussian())
summary(lm14)

lm1=glm(ROM ~ Gabarit , AP, family=gaussian())
summary(lm13)


lm15=glm(OXY ~ Gabarit + ROM + phtalate, AP, family=gaussian())
summary(lm15)

lm16=glm(OXY ~ Gabarit + ROM , AP, family=gaussian())
summary(lm16)

lm16=glm(OXY ~ Gabarit + ROM , AP, family=gaussian())
summary(lm16)

lm16=glm(OXY ~ ROM , AP, family=gaussian())
summary(lm16)

sexcol =ifelse(Appro$Sexe == "M", "blue", "red")
plot(Appro[, 4:10], col=sexcol, pch=19)

mcor = cor(Appro[, 5:10], use = "complete.obs",  method="spearman")
mcor


#############################################################################
#######                      PART B                                  ########
#############################################################################


library(ade4)
ApproNA=na.omit(Appro)
ApproNA
ACP = dudi.pca(ApproNA[,6:10], center=T, scale = T, scannf=F, nf=4)
s.label(ACP$co, xax=1, yax=2)
s.corcircle(ACP$co, xax = 1, yax = 2)
scatter(ACP)


#############################################################################
#######                      PART C                                  ########
#############################################################################

library(ggplot2)
ggplot(Appro, aes (x = Parasite, y = ROM, fill = interaction(Parasite, Sexe)))+geom_boxplot() + geom_line(aes(group=Parasite))

ggplot(Appro, aes (x = Parasite, y = ROM, fill = interaction(Sexe, Parasite)))+geom_boxplot() + geom_line(aes(group=Sexe, linetyp='dashed'))
ggplot(Appro, aes (x = Parasite, y = OXY, fill = interaction(Sexe, Parasite)))+geom_boxplot() + geom_line(aes(group=OXY))

ggplot(Appro, aes (x = Parasite, y = ROM, fill = interaction(Sexe, Parasite)))+geom_boxplot()

+ 
  geom_point(data=Appro, aes (x=Parasite, y=ROM, group =Sexe, colour=Sexe, linetyp='dashed'))+
  geom_line(data=Appro, aes (x=Parasite, y=ROM, group = Sexe, colour=Sexe, linetyp='dashed'))

library(ggplot2)
ggplot(Appro, aes (x = Parasite, y = OXY, fill = interaction(Sexe, Parasite)))+geom_boxplot() + 
  geom_point(data=Appro, aes (x=Parasite, y=ROM, group =Sexe, colour=Sexe, linetyp='dashed'))+
  geom_line(data=Appro, aes (x=Parasite, y=ROM, group = Sexe, colour=Sexe, linetyp='dashed'))



ggplot(data_rat, aes (x = sucre, y = reaction))+geom_boxplot() + 
  geom_point(data=data2, aes (x=treat, y=reac, group =groupe, colour=groupe, linetyp='dashed'))+
  geom_line(data=data2, aes (x=treat, y=reac, group = groupe, colour=groupe, linetyp='dashed'))




library(FactoMineR)
afc=CA(ApproNA[,2:4])


gabarit =na.omit(Appro[5:7]) 
gabarit
shapiro.test(gabarit[,1]) 
shapiro.test(gabarit[,2])
shapiro.test(gabarit[,3])
cor.test(gabarit[,1], gabarit[,2], method="spearman")
cor.test(gabarit[,1], gabarit[,3], method="spearman")
cor.test(gabarit[,2], gabarit[,3], method="spearman")

cor.test(M1[,1], M1[,6], method="pearson")


A = scale(gabarit)
A
B=rowMeans(A)

library(nlme)
lmA=glm(OXY~ Sexe + Parasite, Appro, family=gaussian())
summary(lmA)

Appro4=na.omit(Appro)
Appro4
glm1=glm(glm(ROM ~ Longueure + Poids + Poids_foie + phtalate + OXY, Appro4, family=gaussian())
summary(glm1)

#############################################################################
#######                      PART D                                  ########
#############################################################################


lmB = glm(OXY~ Sexe + Parasite + site, Appro, family=gaussian())
lmB
summary(lmB)
plot(lmB)
lmC=lm(ROM~ Sexe , data=Appro, random = ~1 | site), family=gaussian())
summary(lmC)


lm3= lm(ROM ~  Parasite , random = ~1 | site, data= Appro)
summary(lm3)


lmD=glm(OXY~ Sexe + Parasite + Parasite:site, Appro, family=gaussian())
summary(lmD)

lmE=glm(OXY~ Sexe , Appro, family=gaussian())
summary(lmE)

AP1=na.omit(Appro1[,1:4])
lm2=lm(ROM~Sexe, AP1)

lm3= lme(ROM ~  Sexe + Parasite + Sexe:Parasite, random = ~1 | site, data= AP1)
summary(lm3)
plot(lm3)
lm3= lme(ROM ~  Sexe + Parasite , random = ~1 | site, data= AP1)
summary(lm3)
lm3= lme(ROM ~  Parasite, AP1 )
summary(lm3)
lm3= lme(OXY ~  Sexe , random = ~1 | site, data= AP1)
summary(lm3)

Z=glm(OXY~Parasite, Appro2, family=gaussian())
summary(Z)
AP1

M1 = cor(na.omit(matrix))

glmN = glm(OXY ~ parasite + phtalate + parasite:phtalate, dataa, family=gaussian())
summary(glmN)
library(ggplot2)
ggplot (glmN)

diag(M) = NA
corrplot(M)


#############################################################################
#######                      PART E                                  ########
#############################################################################


plot(dataa[, 5:10], pch=4)



lm20=glm(OXY ~ Parasite + Sexe + parasite:Sexe, Appro, family=gaussian()))

AP2=na.omit(Appro2)
AP2
library(corrplot)
corrplot(AP2, type="upper", order="hclust", tl.col="black", tl.srt=45,)
library(lme4)
b

A=na.omit(Appro)
lm43=lme(ROM ~  Poids + Longueure + Poids_foie + Parasite  , random = ~1 | site, A)
summary(lm43)
lm44=lme(ROM ~  Parasite + Longueure, random = ~1 | site, A)
summary(lm44)
lm1000=lme(ROM ~  Parasite, data=AP2)
AP2

plot(lm43)
par(mfrow = c(2,2))
plot(lm44)

ù
a=coef(lm44)
b=2.5
b
plot(lm44, resid(., type = "p") ~ fitted(.), abline=c(b,az))
plot(lm44, resid(., type = "p") ~ fitted(.), abline(a=)))
library(FactoMineR)
CA(Appro3)
summary(lm1000)
AP2
az=-1.9
az
