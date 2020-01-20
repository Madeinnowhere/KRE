



#Vizsgálat tárgya:
#a várható életkorban bekövetkezett változás milyen egyéb változók változásával függ össze.

#beolvasom a file-t
WHO<-read.csv("C:/Users/Isti/Desktop/life-expectancy-who/WHO.csv", check.names=FALSE, header = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
colnames(WHO)


#leszűröm az adatbázist a két évre ami közötti változásokat fogom vizsgálni
WHO1<-WHO[which(WHO[2]==2014),]
WHO2<-WHO[which(WHO[2]==2001),]

colnames(WHO1)<-paste0(colnames(WHO1),'_2014')
colnames(WHO2)<-paste0(colnames(WHO2),'_2001')

WHO_diff<- cbind(WHO2,WHO1)
#megvizsgálom, hogy az országok ugyanúgy vannak-e mindkét adatbázisban
a<-unlist(WHO_diff$Country_2001) 
b<-unlist(WHO_diff$Country_2014 )
grepl(a[i],b[1])
for( i in 1:length(a)){
   if ((a[i] %in%b[i])==F){
     print(i)
   }
}



#leszűröm a változókat,  a Hepatitis-B vírust ezúzal kihagyom, mert a nagy számú hiányzó adat miatt később jelentősen csökkentené az elemszámot
kell<- c("Country_2001"  ,"Life expectancy_2001" ,"percentage expenditure_2001"  ,"Measles_2001","Polio_2001","Diphtheria_2001"  ,
         "HIV/AIDS_2001","GDP_2001","Life expectancy_2014"  ,"percentage expenditure_2014" , "Measles_2014"    ,
         "Polio_2014"   ,"Diphtheria_2014","HIV/AIDS_2014","GDP_2014"     )
who_diff<- WHO_diff[,kell]
nrow(na.omit(who_diff))
#a hiányzó értékeket kiszűröm
who_diff<- na.omit(who_diff)
colnames(who_diff)
#megalkotom az eltérések data frame-jét
y<-unlist(who_diff[9])
x<-unlist(who_diff[2])
delta_varhatoelet<-(y-x)
delta_varhatoelet<-unname(delta_varhatoelet)
y<-unlist(who_diff["percentage expenditure_2001"])
x<-unlist(who_diff["percentage expenditure_2014"])
delta_egugykiadás<-(x-y)
delta_egugykiadás<-unname(delta_egugykiadás)
y<-unlist(who_diff["Measles_2001"])
x<-unlist(who_diff["Measles_2014"])
delta_Measles<-(x-y)
delta_Measles<-unname(delta_Measles)
y<-unlist(who_diff["Polio_2001"])
x<-unlist(who_diff["Polio_2014"])
delta_Polio<-(x-y)
delta_Polio<-unname(delta_Polio)
y<-unlist(who_diff["Diphtheria_2001"])
x<-unlist(who_diff["Diphtheria_2014"])
delta_Diphtheria<-(x-y)
delta_Diphtheria<-unname(delta_Diphtheria)
y<-unlist(who_diff["HIV/AIDS_2001"])
x<-unlist(who_diff["HIV/AIDS_2014"])
delta_aids<-(x-y)
delta_aids<-unname(delta_aids)
y<-unlist(who_diff["GDP_2001"])
x<-unlist(who_diff["GDP_2014"])
delta_GDP<-(x-y)
delta_GDP<-unname(delta_GDP)
country<-unlist(who_diff$Country_2001)

delta_who <-data.frame(country,delta_varhatoelet,delta_egugykiadás,delta_GDP,delta_aids,delta_Diphtheria,delta_Polio,delta_Measles)

#a két év közötti változások országonként, változónként külön hisztogrammok
par(mfrow=c(3,3))
hist(delta_who[,2],150)
hist(delta_who[,3],150)
hist(delta_who[,4],150)
hist(delta_who[,5],150)
hist(delta_who[,6],150)
hist(delta_who[,7],150)
hist(delta_who[,8],150)
par(mfrow=c(1,1))
colnames(delta_who)<-c("ország" , "varhatoeletkul","eugykiadas", "GDPkul","aidskul" ,"Diphtheriakul","Poliokul", "Measleskul")


# ""Teljesülnek-e a feltételek a modellhez:

# 1. Y folytonos eloszlású változó
# 2. A hiba (reziduális) független eloszlású (nem függ x-től) 
# 3. Sztochasztikusan lineáris az összefüggés Y és X között
# 4. Y normál eloszlású (feltélesen minden X-re)
# 5. Y varianciája nem függ X-től (homoszkedaszticitás)
library(car)
library(psych)
cor(delta_who[,2:8])
KMO(delta_who[,2:8])

#az eloszlások, valamint a korreláció sem megfelelő (a célváltozóval)
library(lmtest)
model.0<-lm(varhatoeletkul ~ GDPkul, data = delta_who)

plot(varhatoeletkul ~ GDPkul, data = delta_who, cex.lab = 1.5)
abline(model.0, col = "red", lwd = 2.5)
legend('bottomright', legend = 'varhatoeletkul ~ GDPkul', lty = 1, col = 'red', lwd = 2.5, title = 'Regression line')

#lefuttatok egy regressziós modellt az aidsre is, mert az korrelál a leginkább a célváltozóval
model.1 <- lm(varhatoeletkul ~  aidskul, data = delta_who)
plot(varhatoeletkul ~  aidskul, data = delta_who, cex.lab = 1.5)
abline(model.1, col = "red", lwd = 2.5)
legend('bottomright', legend = 'varhatoeletkul ~  aidskul', lty = 1, col = 'red', lwd = 2.5, title = 'Regression line')

#nem teljesülnek a feltételek egy lineáris regresszióhoz, nem taláhtaó lináris összefüggés az életkorban
#tapasztalt változások és egyéb változók változásai között


javult_e<-c()
for( i in 1:length(delta_who[,2])){
  if (delta_who[i,2]> 0){
    javult_e<- c(javult_e,1)
  }else{
    javult_e<- c(javult_e,0)
  }
}

delta_who<-cbind(delta_who,javult_e)
#ezek az országok, ahon nem javult a várható életkor
delta_who[which(delta_who$javult_e ==0),]


summary(delta_who$varhatoeletkul)
#átlagosan 4.451-el hosszabbodott a várható életkor, egy logistikus regresszióban tesztelem, hogy mely változó növeli az esélyét, 
#├hogy az átlag feletti értéket érje el
jobbneggyel<-c()
for( i in 1:nrow(delta_who)){
  if (delta_who[i,2] >4.451){
    jobbneggyel<- c(jobbneggyel,1)
  }else{
    jobbneggyel<- c(jobbneggyel,0)
  }
}
delta_who<-cbind(delta_who,jobbneggyel)
#a teszt előtt megalkozom a gyermekolts komponenst
oltas<-delta_who[,6:7]
oltascomp<-prcomp(oltas, scale = TRUE)
summary(oltascomp)# egy komponens két változó információértékének 76%-át magyarázza
delta_who<-cbind(delta_who,scale(oltascomp$x[,1]))
colnames(delta_who)[11]<-"oltascomp"


colnames(delta_who)
model.2<-glm(jobbneggyel~eugykiadas+aidskul+oltascomp+Measleskul, data=delta_who, family = binomial(link = logit))
summary(model.2)
#telszelem a vif mutatóra
vif(glm(jobbneggyel~eugykiadas+aidskul+oltascomp+Measleskul, data=delta_who, family = binomial(link = logit)))
#a vif mutató szerint nincs multikorreláció
#az oltáskompnensnek a két oltás arányának és az aids mértékének időbeni változásának van szignifikánsan hatása
#az esélyhányadosok:
exp(coef(model.2))


#az aids időszakban tapasztalt egységnyi változása 0,82-jére csökkenti az esélyét, hogy a várható életkor az időszakban 
#tapasztalt átlagos változását(azaz 4,451-et) meghaladja.

