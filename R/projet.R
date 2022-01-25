d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)

d3=merge(d1,d2,by=c("Dalc","Walc","school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 370 students

max(d3$Medu)
max(d3$Fedu)


# extraction du echantillon type, on suppose que les étudiants agés de moins de 18 ans ne sont pas très alcooliques car c'est interdit de boire avant cet age
d3 = subset(d3,age >= 18)

# Q1 : alcoolisme par rapport au niveau d'éducation des parents

length(subset(d3,(Fedu+Medu)/2 <= 1 & Dalc>1)$Dalc)/length(subset(d3,(Fedu+Medu)/2 <= 1)$Dalc)
# 0.2307692 d'alcooliques cz modestes
length(subset(d3,(Fedu+Medu)/2 > 1 & Dalc>1)$Dalc)/length(subset(d3,(Fedu+Medu)/2 > 1)$Dalc)
# 0.3766234 d'alcooliques cz non modestes

var(subset(d3,(Fedu+Medu)/2<=1)$Dalc)
# 0.5897436 variance modestes
mean(subset(d3,(Fedu+Medu)/2<=1)$Dalc)
# 1.384615 moyenne modestes

var(subset(d3,(Fedu+Medu)/2>1)$Dalc)
# 1.099111 non modestes

mean(subset(d3,(Fedu+Medu)/2>1)$Dalc)
# 1.649351 moyenne non modestes

modeste<-subset(d3,(Fedu+Medu)/2 <=1)$Dalc

consommation <- rep(1:max(modeste))
vals <- c()
for (a in consommation){
  vals<- append(vals,length(subset(d3,(Fedu+Medu)/2 <=1 & Dalc==a)$Dalc))
}

barplot(vals,ylim=c(0,50),names.arg=consommation,xlab="Degré d'alcoolisme dans la semaine",ylab="Nombre d'étudiants",main="Répartition du degré d'alcoolisme des étudiants avec parents modèstes",col = c("#3B7F02","#5AC500","yellow","orange","red"))

pie(vals , labels = c("1","2","3","4","5"))

# repartition niveau alcoolisme modeste


pasmodeste<-subset(d3,(Fedu+Medu)/2 >1)$Dalc

consommation <- rep(1:max(pasmodeste))
vals <- c()
for (a in consommation){
  vals<- append(vals,length(subset(d3,(Fedu+Medu)/2 >1 & Dalc==a)$Dalc))
}

barplot(vals,ylim=c(0,50),names.arg=consommation,xlab="Degré d'alcoolisme dans la semaine",ylab="Nombre d'étudiants",main="Répartition du degré d'alcoolisme des étudiants avec parents non modèstes",col = c("#3B7F02","#5AC500","yellow","orange","red"))

pie(vals, labels = c("1","2","3","4","5"))

# repartition niveau alcoolisme non modeste


# conclusion Q1 : on remarque que plus les parents sont aisés, plus on a d'alcooliques, on retrouve en effet des 4 et 5 alors que pour les modestes il y en a pas


# Q2 : niveau de reussite par rapport au niveau d'alcoolisme

length(subset(d3,G3.x<10 & Dalc>1)$Dalc)/length(subset(d3,G3.x<10)$Dalc)
# 0.4186047 d'alcooliques majeurs cz les étudiants en echec
length(subset(d3,G3.x>=10 & Dalc>1)$Dalc)/length(subset(d3,G3.x>=10)$Dalc)
# 0.2978723 d'alcooliques majeurs cz les étudiants en echec

var(subset(d3,G3.x<10)$Dalc)
# 0.986711 variance echec
mean(subset(d3,G3.x<10)$Dalc)
# 1.674419 moyenne echec

var(subset(d3,G3.x>=10)$Dalc)
# 1.078631 variance reussite
mean(subset(d3,G3.x>=10)$Dalc)
# 1.553191 moyenne reussite

echec<-subset(d3,G3.x<10)$Dalc

consommation <- rep(1:max(echec))
vals <- c()
for (a in consommation){
  vals<- append(vals,length(subset(d3,G3.x<10 & Dalc==a)$Dalc))
}

barplot(vals,ylim=c(0,35),names.arg=consommation,xlab="Degré d'alcoolisme dans la semaine",ylab="Nombre d'étudiants",main="Répartition du degré d'alcoolisme des étudiants en échec",col = c("#3B7F02","#5AC500","yellow","orange","red"))

pie(vals , labels = c("1","2","3","4","5"))

# repartition niveau alcoolisme étudiants en echec


reussite<-subset(d3,G3.x>=10)$Dalc

consommation <- rep(1:max(reussite))
vals <- c()
for (a in consommation){
  vals<- append(vals,length(subset(d3,G3.x>=10 & Dalc==a)$Dalc))
}

barplot(vals,,ylim=c(0,35),names.arg=consommation,xlab="Degré d'alcoolisme dans la semaine",ylab="Nombre d'étudiants",main="Répartition du degré d'alcoolisme des étudiants en réussite",col = c("#3B7F02","#5AC500","yellow","orange","red"))

pie(vals, labels = c("1","2","3","4","5"))

# repartition niveau alcoolisme étudiants en réussite

# conclusion Q2 : nous remarquons à nouveau une infime difference dans la moyenne, cela s'explique par le grand nombre de 1 qui dominent les données, nous remarquons quand même que pour les étudiants en echec il y a 40% d'alcoliques de 2+ comparé à ceux en reussite où on en a 29%, il y a donc une différence

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

femmes<-subset(d3,sex=="F")$Dalc

consommation <- rep(1:max(femmes))
vals <- c()
for (a in consommation){
  vals<- append(vals,length(subset(d3,sex=="F"  & Dalc==a)$Dalc))
}

barplot(vals,ylim=c(0,40),names.arg=consommation,xlab="Degré d'alcoolisme dans la semaine",ylab="Nombre d'étudiants",main="Répartition du degré d'alcoolisme des étudiantes femmes",col = c("#3B7F02","#5AC500","yellow","orange","red"))

pie(vals , labels = c("1","2","3","4","5"))

# repartition niveau alcoolisme femmes


hommes<-subset(d3,sex=="M")$Dalc

consommation <- rep(1:max(hommes))
vals <- c()
for (a in consommation){
  vals<- append(vals,length(subset(d3,sex=="M" & Dalc==a)$Dalc))
}

barplot(vals,,ylim=c(0,40),names.arg=consommation,xlab="Degré d'alcoolisme dans la semaine",ylab="Nombre d'étudiants",main="Répartition du degré d'alcoolisme des étudiants hommes",col = c("#3B7F02","#5AC500","yellow","orange","red"))

pie(vals, labels = c("1","2","3","4","5"))

# repartition niveau alcoolisme hommes

# conclusion Q3 : nous remarquons que les hommes sont beaucoup plus alcooliques que les femmes, il y a plus d'alcooliques majeurs cz les hommes comparés aux femmes où on a plus de 1.

