# fichier de fusion des données et d'extraction de l'échantillon

d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)

d3=merge(d1,d2,by=c("Dalc","Walc","school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 370 students

max(d3$Medu)
max(d3$Fedu)

# extraction du echantillon type, on suppose que les étudiants agés de moins de 18 ans ne sont pas trés alcooliques car c'est interdit de boire avant cet age
d3 = subset(d3,age >= 18)