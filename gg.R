databas <-read.csv("dataorg.csv")
summary(databas)
#install.packages("ggplot2")
library(ggplot2)

# Rotten.Tomato histrogaram
ggplot(databas, aes( Rotten.Tomato )) +
  geom_histogram(aes(y = ..density..,), color = "#000000", fill = "#E14D2A") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)

# Rotten.Tomato scatterplot
ggplot(databas, aes(x=Rotten.Tomato, y=Worldwide.boxoffice , color=Rotten.Tomato)) + 
  geom_point(size=4)+ geom_smooth(method=lm , color="red", se=TRUE)


boxplot(databas$Rotten.Tomato,
        main = "Movie rotten tomato",
        xlab = "Rotten tomato of 100%",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)





#IMDB histro
ggplot(databas, aes(IMDB)) +
  geom_histogram(aes(y = ..density..,), color = "#98A8F8", fill = "#98A8F8") +
  geom_density(color = "#98A8F8", fill = "#749F82", alpha = 0.6)
#IMDB scatterplot
ggplot(databas, aes(x=IMDB, y=Worldwide.boxoffice , color=Rotten.Tomato)) + 
  geom_point(size=4)+ geom_smooth(method=lm , color="red", se=TRUE)


boxplot(databas$IMDB,
        main = "Movie IMDB",
        xlab = "IMDB of 10",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)








#Production.budget histro

ggplot(databas, aes(Production.budget)) +
  geom_histogram(aes(y = ..density..,), color = "#98A8F8", fill = "#98A8F8") +
  geom_density(color = "#98A8F8", fill = "#749F82", alpha = 0.6)
#Production.budget scatter
ggplot(databas, aes(x=Production.budget, y=Worldwide.boxoffice , color=Rotten.Tomato)) + 
  geom_point(size=4)+ geom_smooth(method=lm , color="red", se=TRUE)


boxplot(databas$Production.budget,
        main = "Movie Production budget",
        xlab = "Production budget",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)








#Opening.weekend.rev
ggplot(databas, aes(Opening.weekend.revenue)) +
  geom_histogram(aes(y = ..density..,), color = "#98A8F8", fill = "#98A8F8") +
  geom_density(color = "#98A8F8", fill = "#749F82", alpha = 0.6)
#scatterplot open week
ggplot(databas, aes(x=Opening.weekend.revenue, y=Worldwide.boxoffice , color=Rotten.Tomato)) + 
  geom_point(size=4)+ geom_smooth(method=lm , color="red", se=TRUE)

boxplot(databas$Opening.weekend.revenue,
        main = "Movie Opening.weekend.theaterst",
        xlab = "Opening.weekend.theaters",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)





#Opening.weekend.theaters
ggplot(databas, aes(Opening.weekend.theaters)) +
  geom_histogram(aes(y = ..density..,), color = "#98A8F8", fill = "#98A8F8") +
  geom_density(color = "#98A8F8", fill = "#749F82", alpha = 0.6)
#scatterplot open week
ggplot(databas, aes(x=Opening.weekend.theaters, y=Worldwide.boxoffice , color=Rotten.Tomato)) + 
  geom_point(size=4)+ geom_smooth(method=lm , color="red", se=TRUE)

boxplot(databas$Opening.weekend.theaters,
        main = "Movie Opening.weekend.theaterst",
        xlab = "Opening.weekend.theaters",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)








#Maximum.theaters
ggplot(databas, aes(Maximum.theaters)) +
  geom_histogram(aes(y = ..density..,), color = "#98A8F8", fill = "#98A8F8") +
  geom_density(color = "#98A8F8", fill = "#749F82", alpha = 0.6)

#Maximum theo scatterplot
ggplot(databas, aes(x=Maximum.theaters, y=Worldwide.boxoffice , color=Rotten.Tomato)) + 
  geom_point(size=4)+ geom_smooth(method=lm , color="red", se=TRUE)

boxplot(databas$Maximum.theaters,
        main = "Movie Maximum.theaters",
        xlab = "Maximum.theaters",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)







#Worldwide.boxoffice
ggplot(databas, aes(Worldwide.boxoffice)) +
  geom_histogram(aes(y = ..density..,), color = "#98A8F8", fill = "#98A8F8") +
  geom_density(color = "#98A8F8", fill = "#749F82", alpha = 0.6)

boxplot(databas$Worldwide.boxoffice,
        main = "Movie Worldwide.boxoffice",
        xlab = "Worldwide.boxoffice",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)








#sum of Genre

library(dplyr)
genereCat <- databas %>%
  group_by(Genre) %>%
  tally()
genre_name <-c("Action","Adventure","Drama","Commedy","Thriller")
genre_categories <- cbind(genereCat,genre_name)
colnames(genre_categories)[1] <-"GenreNoName"
colnames(genre_categories)[2] <-"Number_Of_Genre" 
genre_categories

ggplot(data=genre_categories, aes(x=genre_name, y=Number_Of_Genre,fill=GenreNoName)) +
  geom_bar(stat="identity", color="blue")



#sum of Source

library(dplyr)
data_sou <- databas %>%
  group_by(Source) %>%
  tally()
data_sou
data_sou_name <-c("Original screenplay","Book short story","Based on comic ","Remake","Based on real life events","Based on factual book","other")
sou_categories <- cbind(data_sou,data_sou_name)
colnames(data_sou)[1] <-"SourseNoName"
colnames(data_sou)[2] <-"Number_Of_Sourse" 


ggplot(data=data_sou, aes(x=SourseNoName, y=Number_Of_Sourse,fill=data_sou_name)) +
  geom_bar(stat="identity", color="blue")


#Theoretical distributor


library(dplyr)
data_Thea <- databas %>%
  group_by(Theatrical_distributor) %>%
  tally()
data_Thea
data_Thea_name <-c("Sony pictures","Universel","Lionsgate ","Warner Bros","Walt Disney","Paramount Pictures","10th century Fox","STX Entertainer","Other")
Thea_categories <- cbind(data_Thea,data_Thea_name)
colnames(data_Thea)[1] <-"TheaNoName"
colnames(data_Thea)[2] <-"Number_Of_Thea" 


ggplot(data=data_Thea, aes(x=TheaNoName, y=Number_Of_Thea,fill=data_Thea_name)) +
  geom_bar(stat="identity", color="blue")





#Other  Scatter plots`````````````````````````````````````````````````````````````


#prod budget vs Open weekend rev`

ggplot(databas, aes(x=Production.budget, y=Opening.weekend.revenue)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)


#IMDB vs rotten tomato 
ggplot(databas, aes(x=IMDB, y=Rotten.Tomato)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)

#production budget vs Max theoters
ggplot(databas, aes(x=Maximum.theaters , y=Production.budget)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)

#max theot vs Openning weekend 

ggplot(databas, aes(x=Maximum.theaters , y=Opening.weekend.revenue)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)

#max theot vs IMDB 
ggplot(databas, aes(x=IMDB , y=Maximum.theaters)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)

#max theot vs rotten tomato
ggplot(databas, aes(x=Rotten.Tomato , y=Maximum.theaters)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)



#---------------------------------------------------------------
library(GGally)
data(databas)
head(databas)
ggpairs(databas, columns = c(6,7,14:18,22))




#-------------------------------------------facet box office vs Theoritical--------------------------
data_change <- databas
data_Thea_name <-c("Sony pictures","Universel","Lionsgate ","Warner Bros","Walt Disney","Paramount Pictures","10th century Fox","STX Entertainer","Other")
for(i in 1:9){
  data_change$Theatrical_distributor[data_change$Theatrical_distributor == i] <- data_Thea_name[i]  
}

ggplot(data_change, aes(x=Theatrical_distributor, y=Worldwide.boxoffice)) +
  geom_point(color="steelblue") +facet_wrap(~ Theatrical_distributor)





#-------------------------------------------facet box office vs Genre--------------------------
data_change <- databas
genre_name <-c("Action","Adventure","Drama","Commedy","Thriller")
for(i in 1:5){
  data_change$Genre[data_change$Genre == i] <- genre_name[i]  
}

ggplot(data_change, aes(x=Genre, y=Worldwide.boxoffice)) +
  geom_point(color="steelblue") +facet_wrap(~ Genre)


#-------------------------------------------facet box office vs source--------------------------
data_change <- databas
data_sou_name <-c("Original screenplay","Book short story","Based on comic ","Remake","Based on real life events","Based on factual book","other")

for(i in 1:7){
  data_change$Source[data_change$Genre == i] <- data_sou_name[i]  
}

ggplot(data_change, aes(x=Source, y=Worldwide.boxoffice)) +
  geom_point(color="steelblue") +facet_wrap(~ Source)



