source("pretraitement.R")

#==========================================
# STATISTIQUES ET HYPOTHESES QUESTION N°1 #
#==========================================

# Q1 : alcoolisme par rapport au niveau d'?ducation des parents

length(subset(d3,(Fedu+Medu)/2 <= 1 & Dalc>1)$Dalc)/length(subset(d3,(Fedu+Medu)/2 <= 1)$Dalc)
# 0.2307692 d'alcooliques cz modestes
length(subset(d3,(Fedu+Medu)/2 > 1 & Dalc>1)$Dalc)/length(subset(d3,(Fedu+Medu)/2 > 1)$Dalc)
# 0.3766234 d'alcooliques cz non modestes

var(subset(d3,(Fedu+Medu)/2<=1)$Dalc)
# 0.5897436 variance modestes
mean(subset(d3,(Fedu+Medu)/2<=1)$Dalc)
# 1.384615 moyenne modestes

var(subset(d3,(Fedu+Medu)/2>1)$Dalc)
# 1.099111 variance non modestes

mean(subset(d3,(Fedu+Medu)/2>1)$Dalc)
# 1.649351 moyenne non modestes

# calcul de l'interval de confiance parents non modestes

sd = sqrt(var(subset(d3,(Fedu+Medu)/2>1)$Dalc))
mn = mean(subset(d3,(Fedu+Medu)/2>1)$Dalc)
w1 = subset(d3,(Fedu+Medu)/2>1)$Dalc
error = qt(0.975,df=length(w1)-1)*sd(w1)/sqrt(length(w1))
left <- mean(w1)-error
right <- mean(w1)+error
# interval de confiance entre [1.411396,1.887305]

modeste<-subset(d3,(Fedu+Medu)/2 <=1)$Dalc

consommation1 <- rep(1:max(modeste))
vals1 <- c()
for (a in consommation1){
  vals1<- append(vals1,length(subset(d3,(Fedu+Medu)/2 <=1 & Dalc==a)$Dalc))
}

pasmodeste<-subset(d3,(Fedu+Medu)/2 >1)$Dalc

consommation2 <- rep(1:max(pasmodeste))
vals2 <- c()
for (a in consommation2){
  vals2<- append(vals2,length(subset(d3,(Fedu+Medu)/2 >1 & Dalc==a)$Dalc))
}
# conclusion Q1 : on remarque que plus les parents sont ais?s, plus on a d'alcooliques, on retrouve en effet des 4 et 5 alors que pour les modestes il y en a pas


#==========================================
# STATISTIQUES ET HYPOTHESES QUESTION N°2 #
#==========================================

# Q2 : niveau de reussite par rapport au niveau d'alcoolisme

length(subset(d3,G3.x<10 & Dalc>1)$Dalc)/length(subset(d3,G3.x<10)$Dalc)
# 0.4186047 d'alcooliques majeurs cz les étudiants en echec
length(subset(d3,G3.x>=10 & Dalc>1)$Dalc)/length(subset(d3,G3.x>=10)$Dalc)
# 0.2978723 d'alcooliques majeurs cz les étudiants en rÃ©ussite

var(subset(d3,G3.x<10)$Dalc)
# 0.986711 variance echec
mean(subset(d3,G3.x<10)$Dalc)
# 1.674419 moyenne echec

var(subset(d3,G3.x>=10)$Dalc)
# 1.078631 variance reussite
mean(subset(d3,G3.x>=10)$Dalc)
# 1.553191 moyenne reussite

# calcul de l'interval de confiance etudiants en echec

sd = sqrt(var(subset(d3,G3.x<10)$Dalc))
mn = mean(subset(d3,G3.x<10)$Dalc)
w1 = subset(d3,G3.x<10)$Dalc
error = qt(0.975,df=length(w1)-1)*sd(w1)/sqrt(length(w1))
left <- mean(w1)-error
right <- mean(w1)+error
# interval de confiance entre [1.368716,1.980121]

echec<-subset(d3,G3.x<10)$Dalc

consommation3 <- rep(1:max(echec))
vals3 <- c()
for (a in consommation3){
  vals3<- append(vals3,length(subset(d3,G3.x<10 & Dalc==a)$Dalc))
}

reussite<-subset(d3,G3.x>=10)$Dalc

consommation4 <- rep(1:max(reussite))
vals4 <- c()
for (a in consommation4){
  vals4<- append(vals4,length(subset(d3,G3.x>=10 & Dalc==a)$Dalc))
}

# conclusion Q2 : nous remarquons é nouveau une infime difference dans la moyenne, cela s'explique par le grand nombre de 1 qui dominent les données, nous remarquons quand méme que pour les étudiants en echec il y a 40% d'alcoliques de 2+ comparé é ceux en reussite oé on en a 29%, il y a donc une différence


#==========================================
# STATISTIQUES ET HYPOTHESES QUESTION N°3 #
#==========================================

# Q3 : niveau d'alcoolisme par rapport au sexe

length(subset(d3,sex=="F" & Dalc>1)$Dalc)/length(subset(d3,sex=="F")$Dalc)
# 0.1777778 d'alcooliques majeurs cz les femmes
length(subset(d3,sex=="M" & Dalc>1)$Dalc)/length(subset(d3,sex=="M")$Dalc)
# 0.5333333 d'alcooliques majeurs cz les hommes

var(subset(d3,sex=="F")$Dalc)
# 0.4828283 variance femmes
mean(subset(d3,sex=="F")$Dalc)
# 1.288889 moyenne femmes

var(subset(d3,sex=="M")$Dalc)
# 1.381818 variance hommes
mean(subset(d3,sex=="M")$Dalc)
# 1.933333 moyenne hommes

# calcul de l'interval de confiance etudiants hommes

sd = sqrt(var(subset(d3,sex=="M")$Dalc))
mn = mean(subset(d3,sex=="M")$Dalc)
w1 = subset(d3,sex=="M")$Dalc
error = qt(0.975,df=length(w1)-1)*sd(w1)/sqrt(length(w1))
left <- mean(w1)-error
right <- mean(w1)+error
# interval de confiance entre [1.580172,2.286495]

femmes<-subset(d3,sex=="F")$Dalc

consommation5 <- rep(1:max(femmes))
vals5 <- c()
for (a in consommation5){
  vals5<- append(vals5,length(subset(d3,sex=="F"  & Dalc==a)$Dalc))
}

hommes<-subset(d3,sex=="M")$Dalc

consommation6 <- rep(1:max(hommes))
vals6 <- c()
for (a in consommation6){
  vals6<- append(vals6,length(subset(d3,sex=="M" & Dalc==a)$Dalc))
}

# conclusion Q3 : nous remarquons que les hommes sont beaucoup plus alcooliques que les femmes, il y a plus d'alcooliques majeurs cz les hommes comparés aux femmes oé on a plus de 1.
