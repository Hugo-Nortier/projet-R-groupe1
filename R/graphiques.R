source("pretraitement.R")
source("stats_hypotheses.R")

# Q1 : alcoolisme par rapport au niveau d'éducation des parents

# Répartition du degré d'alcoolisme des étudiants avec parents au niveau d'instruction modestes
barplot(vals1,ylim=c(0,50),names.arg=consommation1,xlab="Degré d'alcoolisme dans la semaine",ylab="Nombre d'étudiants",main="Répartition du degré d'alcoolisme des étudiants avec parents modestes",col = c("#3B7F02","#5AC500","yellow","orange","red"))

# Répartition du degré d'alcoolisme des étudiants avec parents au niveau d'instruction non modestes
barplot(vals2,ylim=c(0,50),names.arg=consommation2,xlab="Degré d'alcoolisme dans la semaine",ylab="Nombre d'étudiants",main="Répartition du degré d'alcoolisme des étudiants avec parents non modestes",col = c("#3B7F02","#5AC500","yellow","orange","red"))

# Q2 : niveau de reussite par rapport au niveau d'alcoolisme

#Répartition du degré d'alcoolisme des étudiants en échec
barplot(vals3,ylim=c(0,35),names.arg=consommation3,xlab="Degré d'alcoolisme dans la semaine",ylab="Nombre d'étudiants",main="Répartition du degré d'alcoolisme des étudiants en échec",col = c("#3B7F02","#5AC500","yellow","orange","red"))

#Répartition du degré d'alcoolisme des étudiants en réussite
barplot(vals4,ylim=c(0,35),names.arg=consommation4,xlab="Degré d'alcoolisme dans la semaine",ylab="Nombre d'étudiants",main="Répartition du degré d'alcoolisme des étudiants en réussite",col = c("#3B7F02","#5AC500","yellow","orange","red"))

# Q3 : niveau d'alcoolisme par rapport au sexe

# Répartition du degré d'alcoolisme des étudiantes femmes
barplot(vals5,ylim=c(0,40),names.arg=consommation5,xlab="Degré d'alcoolisme dans la semaine",ylab="Nombre d'étudiants",main="Répartition du degré d'alcoolisme des étudiantes femmes",col = c("#3B7F02","#5AC500","yellow","orange","red"))

# Répartition du degré d'alcoolisme des étudiants hommes
barplot(vals6,ylim=c(0,40),names.arg=consommation6,xlab="Degré d'alcoolisme dans la semaine",ylab="Nombre d'étudiants",main="Répartition du degré d'alcoolisme des étudiants hommes",col = c("#3B7F02","#5AC500","yellow","orange","red"))
