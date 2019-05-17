
#####################
# Beadando, Radics  #
#####################

csv <- read.csv("C:/Users/Isti/Documents/master.csv", check.names=FALSE, header = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
print(colames(csv))

newhead <- enc2native(colnames(csv))
newhead <- gsub("<[^>]+>", "", newhead)
newhead <- gsub("\n", " ", newhead)
colnames(csv) <- newhead
colnames(csv)


########################################
# Adatok aggregalasa valtozok szerint  #
########################################

#evre,orszagra
df<-aggregate(cbind(population, suicides_no) ~ year + country, data = csv,sum, na.rm=T)
str(df)
df$rata<-df$suicides_no/df$population*100000

#nemre orszagra es evre aggregalva
df2<-aggregate(cbind(population, suicides_no) ~ year + country+ sex, data = csv,sum, na.rm=T)
str(df2)

#nokre,ferfiakra
df_female <- df2[which(df2$sex=="female"),]
df_male <- df2[which(df2$sex=="male"),] 
str(df_female)
str(df_male)

df_female$rata<-df_female$suicides_no/df_female$population*100000
df_male$rata<-df_male$suicides_no/df_male$population*100000

#generaciokra
df3<-aggregate(cbind(population, suicides_no) ~ year+generation , data = csv,sum, na.rm=T)
df3$generation_rata<- df3$suicides_no/df3$population*100000
str(df3)
unique(df3$generation)






######################################################
# Legnagyobb ongyilkossagi rataju orszagokra szures ##
#####################################################

#a
#sima top 25 szurve
top_orszagok<-sort(tapply(df$rata, df$country, mean),decreasing = T)
top_25<-top_orszagok[1:25]
dimnames(top_25)
top25<-as.vector(dimnames(top_25))
top25<-unlist(top25)
top25
df<- df[df$country %in% top25,]
#c
#noi top 10-re szurve
top_orszagok_female<-sort(tapply(df_female$rata, df_female$country, mean),decreasing = T)
top_10f<-top_orszagok_female[1:10]
top10f<-as.vector(dimnames(top_10f))
top10f<- unlist(top10f)
df_female<- df_female[df_female$country %in% top10f,]
#b
#ferfi top10-re szurve
top_orszagok_male<-sort(tapply(df_male$rata, df_male$country, mean),decreasing = T)
top_10m<-top_orszagok_male[1:10]
top10m<-as.vector(dimnames(top_10m))
top10m<- unlist(top10m)
df_male<- df_male[df_male$country %in% top10m,]
#d
#top 6 orszag generacionkent
dfg<-aggregate(cbind(population, suicides_no) ~ year + country+generation, data = csv,sum, na.rm=T)
dfg$rata<-dfg$suicides_no/dfg$population*100000
top_6<-top_orszagok[1:6]
top6<-as.vector(dimnames(top_6))
top6<-unlist(top6)
df6<- dfg[dfg$country %in% top6,]
df6$generation<- factor(df6$generation)
unique(df6$generation)

##############
# Abrazolas  #
##############

#par(mfrow=c(3,2)) 
par(mar=c(6,6,6,6),xpd=F)

library("ggplot2")

#a
# top 25    

x_axis_labels <- min(df$year):max(df$year)

g<-ggplot(df,aes(x=year, y=rata,group=country))
g<-g+geom_line(aes(col=country),size=1)+
scale_color_manual(values=c(
"#E20606", "#E23506","#3a313a","#E28A06","#E2D306","#98E206",
"#5EE206","#0f1a26","#00e2b6","#f25d0c","#0fac82","#ac330f","#36bce2",
"#0f88ac","#224e44","#ff906a","#2c91cc","#2e613b","#36433e","#6b4700","#731d1d",
"#794044","#2ae206","#2706e2","#000000"))+
labs(title="Legmagasabb 25 öngyilkosságirátával rendelkezõ ország 1985-2016",x="Év",y="Százezer fõre jutó öngyilkosságok száma")+
scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)+
theme(
    panel.background = element_rect(fill = "White",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey72"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "White"),
    legend.direction="horizontal", 
    legend.position="bottom",
    legend.text = element_text(color = "black", size = 7),
    legend.key.size = unit(0.2, "cm"),
    legend.key.width = unit(1.5,"cm"),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 75, vjust = 0.5, face="bold", color="black"),
    axis.text.y = element_text(face="bold", color="black"))

g  

#c
#top10 nokre
g1<-ggplot(df_female,aes(x=year, y=rata,group=country))
g1<-g1+geom_line(aes(col=country),size=1)+
  scale_color_manual(values=c(
    "#E20606","#7fff8e","#0f1a26","#0f88ac","#e06868","#ff906a","#794044","#2ae206","#2706e2","#f2db09"))+
  labs(title="Legmagasabb 10  öngyilkosságirátával rendelkezõ ország 1985-2016",
       subtitle = "Nõkre",
       x="Év",y="Százezer fõre jutó öngyilkosságok száma")+
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)+
  theme(
    panel.background = element_rect(fill = "White",
                                    colour = "grey72",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey72"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "White"),
    legend.direction="horizontal", 
    legend.position="bottom",
    legend.title = element_blank(),
    legend.text=element_text(size=7),
    axis.text.x = element_text(angle = 75, vjust = 0.5, face="bold", color="black"),
    axis.text.y = element_text(face="bold", color="black"))
g1


#b
#top10 ferfiakra
g2<-ggplot(df_male,aes(x=year, y=rata,group=country))
g2<-g2+geom_line(aes(col=country),size=1)+
  scale_color_manual(values=c(
    "#E20606","#7fff8e","#0f1a26","#0f88ac","#e06868","#ff906a","#794044","#2ae206","#2706e2","#f2db09"))+
  labs(title="Legmagasabb 10  öngyilkosságirátával rendelkezõ ország 1985-2016",
       subtitle = "Férfiakra",
       x="Év",y="Százezer fõre jutó öngyilkosságok száma")+
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)+
  theme(
    panel.background = element_rect(fill = "White",
                                    colour = "grey72",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey72"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "White"),
    legend.direction="horizontal", 
    legend.position="bottom",
    legend.text=element_text(size=7),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 75, vjust = 0.5, face="bold", color="black"),
    axis.text.y = element_text(face="bold", color="black"))
g2

#d
#Generaciokra idosor
g3<-ggplot(df3,aes(x=year, y=generation_rata,group=generation))
g3<-g3+geom_line(aes(col=generation),size=1)+
  scale_color_manual(values=c(
    "#E20606","#7fff8e","#0f1a26","#0f88ac","#2706e2","#f2db09"))+
labs(title="Öngyilkossági ráta generációkra vetítve 1985-2016",
     x="ÉV",y="Százezer fõre jutó öngyilkosságok száma")+
scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)+
geom_text(data = df3, label= round(df3$generation_rata, 1),
          size = 3, check_overlap = TRUE,
          colour="black",angle=65)+
theme(
    panel.background = element_rect(fill = "White",
                                    colour = "White",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "White"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "White"),
    legend.direction="horizontal", 
    legend.position="bottom",
    legend.text=element_text(size=7),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 75, vjust = 0.5, face="bold", color="black"),
    axis.text.y = element_text(face="bold", color="black"))
g3
 

#f
#Generraciok barchart top 6 orszagra            


g4<-ggplot(data=df6, aes(x=country, y=rata, fill=factor(generation, levels=c("G.I. Generation","Silent","Boomers","Generation X","Millenials","Generation Z" )))) +
  geom_bar(stat="identity", width=0.5,position=position_dodge())+
  scale_fill_manual(values=c("#FF8C00", "#A05D56", "#6B486B","#7B6888","#8A89A6","#98ABC5"))+
  labs(title="Legmagasabb öngyilkossági rátávalval rendelkezõ hat oszrág 1985-2016",
       x="Ország",y="Százezer fõre jutó öngyilkosságok száma",
       subtitle="Generációnként")+
  theme(
    panel.background = element_rect(fill = "White",
                                    colour = "White",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "White"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "White"),
    legend.position=c(0.9,0.9),
    legend.title = element_blank(),
    legend.text=element_text(size=7),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.3,"cm"),
    axis.text.y = element_text(color="grey40"))
g4  
#e
#top6 oszlop
g5<-ggplot(data=df6, aes(x=year, y=rata, fill=country)) +
  geom_bar(stat="identity", width=0.5,position=position_dodge())+
  scale_fill_manual(values=c("#FF8C00", "#A05D56", "#6B486B","#7B6888","#8A89A6","#98ABC5"))+
  labs(title="Legmagasabb öngyilkossági rátávalval rendelkezõ hat oszrág 1985-2016",
       x="Év",y="Százezer fõre jutó öngyilkosságok száma")+
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)+
  theme(
    panel.background = element_rect(fill = "White",
                                    colour = "White",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "White"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "White"),
    legend.position=c(0.9,0.85),
    legend.title = element_blank(),
    legend.text=element_text(size=7),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.3,"cm"),
    axis.text.x = element_text(angle = 75, vjust = 0.5, color="grey40"),
    axis.text.y = element_text(color="grey40"))
g5
  

#############
#egybefuzes#
###########
#install.packages("ggpubr")

#library("ggpubr")

a<-g
b<-g2
c<-g1
d<-g3
e<-g5
f<-g4
#suicide_plot<- ggarrange(a,b,c,d,e,f,ncol=3,nrow=2)


par(mar=c(1,1,1,1),xpd=T)
#install.packages("gridExtra")
library(gridExtra)
plotos<-grid.arrange(a,b,c,d,e,f,ncol=3,nrow=2)


