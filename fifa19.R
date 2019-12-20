#Az adata valódi football játékosok modellezésének tekinthető
# Elemzés célja: teljesítmény hatásának vizsgálata a játékos értékére vonatkozóan


#Beolvasom a file-t

fifa <- read.csv("C:/Users/Isti/Downloads/fifa19/data.csv",
                  check.names=FALSE, header = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
colnames(fifa)
str(fifa)
nrow(fifa)
fifa["Value"]
#sornévnek beállítom a játékosok nevét, duplikáció miatt sorsszámmal egybefűzve
nevek<-unlist(fifa["Name"])
nevek1<-c()

mind1<-c()
for (i in 1: length(nevek)){
  mind1<-paste0(nevek[i],"_",i)
  nevek1<-c(nevek1,mind1)
  mind<-c()
 
}
nevek1
rownames(fifa)<-nevek1

#megnézem, hogy mindenkinek érvényes-e a szerződése
fifa[,26]
str(fifa[,26])
unique(unlist(regmatches(fifa[,26], gregexpr("[[:digit:]]+", fifa[,26]))))
#teljesítménymutatókból kreálok pc-ket, de előtte kiszűröm a kapusokat
unique(fifa["Position"])

fifafield<- fifa[which(fifa["Position"]!="GK"),]
nrow(fifafield)
#Null értékeket kiszedem, hogy értelmezhető legyen a korreláció
fifafield<-na.omit(fifafield)
kell1<- c( "Crossing",                 "Finishing",               
 "HeadingAccuracy",          "ShortPassing",             "Volleys",                  "Dribbling"  ,             
 "Curve",                    "FKAccuracy" ,              "LongPassing"  ,            "BallControl" ,            
 "Acceleration",             "SprintSpeed",              "Agility"      ,          "Reactions"      ,         
 "Balance",                  "ShotPower" ,               "Jumping"       ,           "Stamina"       ,          
 "Strength",                 "LongShots" ,               "Aggression"     ,          "Interceptions"  ,         
 "Positioning",              "Vision"    ,               "Penalties"       ,         "Composure"  )

library(psych)
 perform<-fifafield[,kell1]


lapply(perform,mean)
lapply(perform,sd)
#futtatok pár eloszlást

par(mfrow=c(5,2))
lapply(perform,hist)
par(mfrow=c(1,1))
#korreláció
cor(perform)
KMO(perform) 
#componensek meghatározása
?(prcomp)
perfcomp<-prcomp(perform, scale = TRUE)
summary(perfcomp)

#2 komponenst tartok meg
library(ellipse)
#install.packages("factoextra")
library(factoextra)
#install.packages("rlang")
#install.packages("purrr")
library(ggplot2)

fviz_pca_var(perfcomp,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
varimax(perfcomp$rotation[, 1:2])
#valuek megtisztítása
colnames(fifafield)
str(fifafield["Value"])
fifafield[,"Value"]

ertek<- enc2native(fifafield[,"Value"])
ertek <- gsub("€", "", ertek)
ertek <- gsub("M", "000000", ertek)
ertek <- gsub("K", "000", ertek)




for (i in 1:length(ertek)){
if (grepl("\\.",ertek[i])==T){
  ertek[i]<- gsub(".$","",ertek[i])
  ertek[i]<- gsub("\\.","",ertek[i])                
  
  }
}
ertek<-as.numeric(ertek)
summary(ertek)
hist(ertek)
min(ertek)

#hozzáfűzöm a dataframehez, a célváltozót, és a két komponenst standardizálva
fifafield<-cbind(fifafield,ertek,scale(perfcomp$x[,1]),scale(perfcomp$x[,2]))

colnames(fifafield)[91]<-"csapatjatekcomp"
colnames(fifafield)[92]<-"eronletcomp"
colnames(fifafield)
sd(fifafield$eronletcomp)
sd(fifafield$csapatjatekcomp)
fifafield$eronletcomp
fifafield$csapatjatekcomp
#kiszedem a 0 értékeket
fifafield<-fifafield[which(fifafield["ertek"]!=0),]
hist(fifafield[,"ertek"])
head(sort(fifafield[,"ertek"], decreasing=TRUE), 50)
tail(sort(fifafield[,"ertek"], decreasing=TRUE), 10)


#ellenörzöm, hogy Gamma eloszlást követ-e a célváltozó
plot(fifafield[,"ertek"], log="y", type='h')
summary(glm(fifafield[,"ertek"]~1,family=Gamma))


#tesztelem glm-ben a teljesítményváltozókból készített komponenseket
model.0<-glm(ertek~eronletcomp+csapatjatekcomp,family=Gamma(link=log), data=fifafield )
summary(model.0)

#################################################
#Az eredmény multiplikatív és nem additív  mert:E[y|z,x+1]=exp(α+β⋅(x+1)+γ⋅z)=exp(α+β⋅x+β+γ⋅z)=exp(α+β⋅x+γ⋅z)⋅exp(β)=E[y|z,x]⋅exp(β)      #
exp(coef(model.0))

#Tehát míg az erőnlétkomponens körülbelül egységi varianciája  növeli a játékos értékét, a csapatjátéké.
# Lehetséges ok: azok akik inkáb csapatjátékra összpontosítanak ott az egyéni teljesítmény kevésbé hangsúlyos
# és ez befolyásolhatja a játékos értékét. Persze ez csak feltételezés, további kutatások célja lehet vizsgálni a jelenséget.
ered <- predict(model.0, type="response")
cor(ered, fifafield[,"ertek"])^2
#Tehát a két teljesítmény komponens 53% információt magyaráz a játékos értékével kapcsolatban.
                                               
#a modellt kiegészítem a korváltozóval

cor(fifafield[,"Age"],fifafield["eronletcomp"])
cor(fifafield[,"Age"],fifafield[,"csapatjatekcomp"])
#a kort standardizálva hozzárendelem a daraframe-hez
fifafield<- cbind(fifafield,scale(fifafield[,"Age"]))
colnames(fifafield)[93]<-"korstandard"
colnames(fifafield)
sd(fifafield$korstandard)
fifafield$korstandard

model.1 <- update(model.0, . ~ . + korstandard, data=fifafield)
summary(model.1)
exp(coef(model.1))
ered <- predict(model.1, type="response")
cor(ered, fifafield[,"ertek"])^2
#Tehát a kor is csökkenti a játékos értékét, nem csak a csapatjáték komponens, a kor kicsit erősebben is hat.
#Az információ további 5%-át tudtuk magyarázni a kor segítségével.


AIC(model.0, model.1)
#a modell a kor bevonásával jobban teljesít az AIC mutató szerint.
#################################



#bérekre is megnézem a komponenseket

ber2<-fifafield[,"Wage"]
ber2
ber2 <- gsub("€", "", ber2)
ber2<- gsub("M", "000000", ber2)
ber2 <- gsub("K", "000", ber2)


for (i in 1:length(ber2)){
  if (grepl("\\.",ber2[i])==T){
    ber2[i]<- gsub(".$","",ber[i])
    ber2[i]<- gsub("\\.","",ber[i])                
    
  }
}

ber2<- as.numeric(ber2)
hist(ber2,100)

fifafield<-cbind(fifafield,ber2)

fifafield<-fifafield[which(fifafield["ber2"]!=0),]
plot(fifafield[,"ber2"], log="y", type='h')
hist(fifafield[,"ber2"])
head(fifafield[,"ber2"],decreasing=T,10)
tail(fifafield[,"ber2"],decreasing=T,10)


#modell a bérekre
model.2<-glm(ber2~eronletcomp+csapatjatekcomp,family=Gamma(link=log),data= fifafield )
summary(model.2)
exp(coef(model.2))
ered <- predict(model.2, type="response")
cor(ered, fifafield[,"ertek"])^2

# A bérekre hasonlóan hat a két komponens, mint a játékos értékénére, azzal a különbséggel, hogy a hatás picivel kevésbé jelentős, a
# coeficiensek exponális értéke mind két esetben ezúttal közelebb van az egyhez . A komponensek a bérekre vonatkozó információ 50%-át magyarázzák.

# Szintén megvizsgálom a modelt kiegészítve a kor változóval
model.3 <- update(model.2, . ~ . + korstandard, data=fifafield)
summary(model.3)
exp(coef(model.3))
ered <- predict(model.3, type="response")
cor(ered, fifafield[,"ertek"])^2
# A kor kisebb mértékben de csökkenti a fizetést, csakúgy mint a csapatjték komponens.
# A változó bevonásával a modell közel 5%-al több információt magyaráz.
AIC(model.2,model.3) 
# A model a kor bevonsásval az Akaike' információs kritérium szerint jobban teljesít.



min(fifafield[,"Age"])
max(fifafield[,"Age"])
katkor<-cut(fifafield[,"Age"], breaks = c(19,20,25,30,35, 39), labels= c("16-19","20-24","25-29","30-34","35+"),right = F )
fifafield<-cbind(fifafield,katkor)


#linearitás vizsgálata jövedelemre és játékos értékére vonatkozóan életkor tekintetében, ANOVA teszttel
anova1<-aov(ber2 ~ katkor, data = fifafield)
summary(anova1)
TukeyHSD(anova1)

anova2<-aov(ertek ~ katkor, data = fifafield)
summary(anova2)
TukeyHSD(anova2)

#amint látjuk  legfiatalabb játékosokcsoportbak minden esetben szignifikánsan alacsonyabb a bére és értéke. 
# a többi korosztály közöttt pedig nincs egyértelmű linearitás. 
# Tehát a kor hatását a modellben nem érdemes lineárisnak tekinteni


