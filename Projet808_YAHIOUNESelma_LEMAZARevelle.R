### Installations si nécéssaire ###

#install.packages("corrplot")
#install.packages("viridis")
#install.packages("ggplot2")
#install.packages("rworldmap")
#install.packages("dplyr")
#install.packages("ggpubr")


library(corrplot)
library(viridis) 
library(ggplot2)
library(rworldmap)
library(dplyr)
library(ggpubr)



##### LECTURE DE LA DATASET #####
path <- file.choose()
data<- read.csv(path)
summary(data)


##### MATRICE DE CORRELATION ##### 

corrplot(cor(data[3:9]), tl.col = "darkgray",title="Matrice de correlation", tl.srt = 30, bg = "White",
         type = "full",col=viridis(11),mar=c(0,0,1,0))

####### NUAGES DE POINTS ####### 

###### ETUDE DES RELATIONS AVEC LE SCORE ###### 

##### SCORE EN FONCTION DU PIB ##### 

ggplot(data)+aes(x=GDP.per.capita, y=Score)+geom_point(color="#563291")+geom_smooth(method="lm", se=FALSE,color="pink")

##### SCORE EN FONCTION DU SUPPORT SOCIAL ##### 

ggplot(data)+aes(x=Social.support, y=Score)+geom_point(color="#105363")+geom_smooth(method="lm", se=FALSE,color="#8dc6b5")

##### SCORE EN FONCTION DE L'ESPERANCE DE VIE ##### 

ggplot(data)+aes(x=Healthy.life.expectancy, y=Score)+geom_point(color="#29d0f9")+geom_smooth(method="lm", se=FALSE,color="#105363")

##### SCORE EN FONCTION DE LA GENEROSITE ##### 
#(Plus faible corrélation avec le score dans la matrice de corrélation)

ggplot(data)+aes(x=Generosity, y=Score)+geom_point(color="darkgreen")+geom_smooth(method="lm", se=FALSE,color="darkgray")

###### ETUDE DES AUTRES VARIABLES FORTEMENT CORRELEES ###### 

##### GDP + HEALTHY LIFE EXPECTANCY ##### 
ggplot(data)+aes(x=GDP.per.capita, y=Healthy.life.expectancy,color=Score)+geom_point()+geom_smooth(method="lm", se=FALSE)+scale_color_viridis(option="viridis")    

##### GDP + SOCIAL SUPPORT ##### 
ggplot(data)+aes(x=GDP.per.capita, y=Social.support,color=Score)+geom_point()+geom_smooth(method="lm", se=FALSE,col="#FA8072")+scale_color_viridis(option="magma")    


##### SOCIAL SUPPORT + HEALTHY LIFE EXPECTENCY ##### 

ggplot(data)+aes(x=Social.support, y=Healthy.life.expectancy,color=Score)+geom_point()+geom_smooth(method="lm", se=FALSE,col="#FA8072")+scale_color_viridis(option="inferno") 

##### GENEROSITY ET HEALTHY LIFE EXPECTANCY ##### 
### (Valeur de coefficient la plus petite dans la matrice de corrélation)

ggplot(data)+aes(x=Healthy.life.expectancy, y=Generosity)+geom_point(color="#900C3F")+geom_smooth(method="lm", se=FALSE,color="#de83ab")
#--> Très faible corrélation linéaire 

####### CARTES GRAPHIQUES ####### 

##### CARTE 1 : SCORE #####

carte <- data.frame(
  Pays=data$Country.or.region,
  Score=data$Score)

n <- joinCountryData2Map(carte, joinCode="NAME", nameJoinColumn="Pays")
mapCountryData(n, nameColumnToPlot="Score", mapTitle="Carte du monde représentant les pays et leurs scores")


##### Carte 2 : PIB #####

carte <- data.frame(
  Pays=data$Country.or.region,
  PIB=data$GDP.per.capita)

n <- joinCountryData2Map(carte, joinCode="NAME", nameJoinColumn="Pays")
mapCountryData(n, nameColumnToPlot="PIB", mapTitle="Carte du monde des pays et leurs PIB",colourPalette=terrain.colors(7,rev=TRUE))

##### Carte 3 : FREEDOM OF CHOICE #####

carte <- data.frame(
  Pays=data$Country.or.region,
  Freedom=data$Freedom.to.make.life.choices)

n <- joinCountryData2Map(carte, joinCode="NAME", nameJoinColumn="Pays")
mapCountryData(n, nameColumnToPlot="Freedom", mapTitle="Représentation de la liberté de choix des pays du monde",colourPalette=blues9)


##### HISTOGRAMME DE DENSITE : GENEROSITY ##### 

ggplot(data)+aes(x=Generosity) + 
  geom_histogram(aes(y=..density..), colour="darkblue", fill="lightblue")+
  geom_density(alpha=.2, fill="black") +  geom_vline(aes(xintercept=mean(Generosity)),color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=max(Generosity)),color="purple", linetype="dashed", size=1)


########## PARTIE II  #####################

nd=data[data[['Country.or.region']] %in% c('Germany','Norway','Brazil','India','Somalia','Malawi'),]

##### PIE CHART : SCORE #####

sd=nd[order(nd$Score),]
sd <- sd %>%
  arrange(desc(Overall.rank)) %>%
  mutate(prop = round(Score*100/sum(Score), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)


ggpie(
  sd, x="Score",label = "Score",
  lab.pos = c("in"), lab.font = list(color = "black"), 
  fill = "Country.or.region", color = "white",
  palette = "BrBG",
  title="Représentation en camembert des scores"
)


##### PIE CHART 2 : ESPERANCE DE VIE #####
sd <- sd %>%
  arrange(desc(Overall.rank)) %>%
  mutate(prop = round(Healthy.life.expectancy*100/sum(Healthy.life.expectancy), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)

ggpie(
  sd, x="Healthy.life.expectancy",label = "Healthy.life.expectancy",
  lab.pos = c("out"), lab.font = list(color = "#34495e"), 
  fill = "Country.or.region", color = "white",
  palette = "Spectral",
  title="Représentation en camembert de \n      l'espérance de vie des pays"
) 

##### AJOUT DE LA VARIABLE TYPE #####

sd[sd[['Country.or.region']] %in% c('Germany','Norway'),'Type']="Développé"
sd[sd[['Country.or.region']] %in% c('Brazil','India'),'Type']="Emergent"
sd[sd[['Country.or.region']] %in% c('Somalia','Malawi'),'Type']="En voie de développement"

###### DIAGRAMME EN BATON : PAYS PIB TYPE ########
ggplot(sd, aes(x = Country.or.region, y = GDP.per.capita))+geom_col(aes(fill=Type) )+scale_fill_manual(values=c("darkorange","red","yellow")) +
  geom_text(aes(label = GDP.per.capita, vjust = -0.3))+ggtitle("Diagramme en baton des PIB de chaque pays")+theme(plot.title = element_text(hjust = 0.5))


##### DIAGRAMME EN BATON : PAYS PIB SCORE TYPE #####

ggplot(sd, aes(x = GDP.per.capita, y =Score ))+geom_col(aes(fill=Type) )+scale_fill_manual(values=c("red","darkorange","yellow")) +
  geom_text(aes(label = Country.or.region, vjust = -0.3))+ggtitle("Diagramme en batons des scores de chaque pays en fonction de leur PIB")+theme(plot.title = element_text(hjust = 0.5))



##### NUAGE DE POINT ET DROITE DE REGRESSION : PAYS PIB SCORE TYPE #####

ggplot(sd, aes(x = GDP.per.capita, y =Score ))+geom_point(aes(fill=Type),,shape=23,size=3 )+scale_fill_manual(values=c("red","darkorange","yellow")) +
  geom_text(aes(label = Country.or.region, vjust = -0.3))+ggtitle("Représentation en points des scores de chaque pays \nen fonction de leur PIB")+theme(plot.title = element_text(hjust = 0.5))+geom_smooth(method="lm", se=FALSE,col="brown")


##### DIAGRAMME EN BATON (LOLLIPOP) : PAYS CORRUPTION TYPE #####

ggplot(sd, aes(Country.or.region, Perceptions.of.corruption)) +
  geom_linerange(
    aes(x = Country.or.region, ymin = 0, ymax = Perceptions.of.corruption), 
    color = "lightgray", size = 1.5
  )+
  geom_point(aes(color=Type), size = 2)+
  ggpubr::color_palette("npg")+
  theme_pubclean()

