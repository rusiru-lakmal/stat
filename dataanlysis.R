
database <-read.csv("films.csv")
summary(database)

datadf <- as.data.frame(database)
head(datadf)
table(datadf$Theatrical.distributor)

#qulitative to numerical data
#Theatrical.distributor

datadf$Theatrical.distributor <- as.factor(datadf$Theatrical.distributor)

datadf$Theatrical.distributor <- unclass(datadf$Theatrical.distributor);
datadf$Theatrical.distributor

#$Genre
datadf$Genre <- as.factor(datadf$Genre)

datadf$Genre <- unclass(datadf$Genre)

#Production.method
datadf$Production.method <- as.factor(datadf$Production.method)

datadf$Production.method <- unclass(datadf$Production.method)

#Creative.type
datadf$Creative.type<- as.factor(datadf$Creative.type)

datadf$Creative.type <- unclass(datadf$Creative.type)


datadf$Source<- as.factor(datadf$Source)

datadf$Source <- unclass(datadf$Source)

head(datadf)
