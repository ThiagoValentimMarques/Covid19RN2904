######################################################################################
#                                                                                    #
# CÓDIGOS UTILIZADOS NO POST - ANÁLISE DESCRITIVA DOS DADOS SOBRE A COVID-19 NO RN   #
#                                                                                    #
# AUTOR: THIAGO VALENTIM                                        DATA: 29/04/2020     #
#                                                                                    #
######################################################################################

#-------------------------------- PACOTES NECESSÁRIOS -------------------------------#

library(tidyverse)
library(brazilmaps)
library(ggrepel)
library(gridExtra)
library(lubridate)
library(ggspatial)
#--------------------------------------- DADOS --------------------------------------#

setwd("C:\\Users\\Thiago\\Desktop")

dados <- read.csv("COVID19_20200428.csv",header=TRUE,sep=";") 


########## INFORMAÇÕES DA POPULAÇÃO DE CADA ESTADO DO NORDESTE ###################

n <- length(dados[dados$estado=="RN",]$estado)

nordeste <- rep(c("RN","PB","PE","SE","MA","CE","BA","PI","AL"),each=n)

pop_ne <- rep(c(3409000,3944000,9278000,2220000,6851000,8843000,15130000,3195000,
                3322000),each=n)

dia<-rep(substr(dados[dados$estado =="RN",]$data,6,10),9)

info <- data.frame(nordeste,pop_ne,dia)

hoje <- "04-28"  #deve ser inserida a data de hoje em mês-dia
hoje2<- as.Date("28/04/2020",format="%d/%m/%y") #Data de hoje "dd/mm/aaaa"

dados <- as_tibble(dados)

dados <- dados %>%   
  mutate(data = substr(data,6,10))%>%
  mutate(label = if_else(data == hoje,
                         as.character(estado), NA_character_))


########################### GRÁFICO 1

dados %>%
  group_by(data) %>%
  filter(estado %in% c("RN"))%>%
  filter(casosAcumulados>0)%>%
  ggplot(., aes(x = data, y = casosAcumulados, group = estado, colour = estado)) +
  geom_line(aes(y = obitosAcumulados, group="",colour = "Casos Acumulados"))+
  geom_point(aes(y = obitosAcumulados, group="",colour = "Casos Acumulados"))+
  geom_point()+geom_line()+
  scale_color_manual(labels = c("Óbitos","Casos confirmados"),values=c("red", "blue"))+
  labs(x = "Data", y = "Quantidade", colour = "",
       caption="Fonte: Ministério da Saúde    Autor: Thiago Valentim",
       title="Figura 1: Acumulado dos casos confirmados e óbitos por Covid-19 no RN") +
  theme(legend.position="bottom", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  annotate("text",x=2,y=790,hjust=0,vjust=0,
           label=paste("Letalidade =","14%"),colour="red",size=4.5)

########################### GRÁFICO 2

dados %>%
  group_by(data) %>%
  filter(estado %in% c("RN"))%>%
  filter(casosAcumulados>0)%>%
  ggplot(., aes(x = data, y = casosNovos, group = estado, colour = estado)) +
  geom_line(aes(y = obitosNovos, group="",colour = "Casos Acumulados"))+
  geom_point(aes(y = obitosNovos, group="",colour = "Casos Acumulados"))+
  geom_point()+geom_line()+
  scale_color_manual(labels = c("Óbitos","Casos confirmados"),values=c("red", "blue"))+
  labs(x = "Data", y = "Quantidade", colour = "",
       caption="Fonte: Ministério da Saúde    Autor: Thiago Valentim",
       title="Figura 2: Casos confirmados e óbitos diários por Covid-19 no RN") +
  theme(legend.position="bottom", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))
  
########################### GRÁFICO 3

c<-dados %>%
  group_by(data) %>%
  filter(estado %in% c("RN"))%>%
  filter(casosAcumulados>0)%>%
  ggplot(., aes(x = data, y = obitosAcumulados, group = estado, colour = estado)) +
  geom_point()+geom_line()+
  scale_color_manual(labels = c("Casos confirmados", "Óbitos"),values=c("red"))+
  labs(x = "Data", y = "Quantidade", colour = "",
       caption=" ",
       title="Figura 3: (a) Acumulado dos óbitos") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

########################### GRÁFICO 3

d<-View(dados %>%
  group_by(data) %>%
  filter(estado %in% c("RN"))%>%
  filter(casosAcumulados>0))%>%
  ggplot(., aes(x = data, y = obitosNovos, group = estado, colour = estado)) +
  geom_line(aes(y = obitosNovos))+
  geom_point()+geom_line()+
  scale_color_manual(labels = c("Óbitos"),values=c("red"))+
  labs(x = "Data", y = "Quantidade", colour = "",
       caption="Fonte: Ministério da Saúde    Autor: Thiago Valentim",
       title="(b) Óbitos diários por Covid-19 no RN") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

grid.arrange(c,d,ncol=2)

######################################################################################

#################### Mapa - Figura 4 ##############################################

mapa <- get_brmap("City", geo.filter = list(State = 24))
glimpse(mapa)

dim(mapa)

mapa[,8]<-rep(0,dim(mapa)[1])
names(mapa)[8]<-"obitos"

nomes1<-c("PARNAMIRIM","MACAÍBA","APODI","CEARÁ-MIRIM","ENCANTO",
          "IPANGUAÇU","TOUROS","CERRO CORÁ","NÍSIA FLORESTA",
          "SÃO RAFAEL","ALEXANDRIA","TAIPU","CARNAÚBA DOS DANTAS",
          "LAGOA DE PEDRAS")

mapa[mapa$nome %in% nomes1,8]<-rep(1,14)

nomes2<- c("AÇU","TENENTE ANANIAS","SÃO GONÇALO DO AMARANTE")        

mapa[mapa$nome %in% nomes2,8]<-c(2,2,2)  

nomes3<- c("CANGUARETAMA","MOSSORÓ","NATAL")        

mapa[mapa$nome %in% nomes3,8]<-c(3,10,11) 

mapa$obitos<- as.numeric(mapa$obitos)

mapa %>% 
  ggplot()+ geom_sf(aes(fill = obitos))+
  labs(fill = "Total de óbitos",
       title="Figura 4: Mapa dos óbitos por Covid-19 no RN",
       subtitle ="27/04/2020", 
       caption="Fonte: LAIS/HUOL/UFRN    Autor: Thiago Valentim")+
  scale_fill_gradient(low="white", high="red3")+
  theme(panel.grid = element_line(colour = "grey95"),
        legend.position = "bottom",
        legend.text = element_text(size=6))+
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.08, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = c(-38.5, -34.9), ylim = c(-7.1, -4.8))

#######################################################################################

############# Figura 5

dados %>%
  filter(estado %in% c("RN","PB","PE","SE","MA","CE","BA","PI","AL"))%>%
  left_join(info, c("data" = "dia","estado"="nordeste")) %>%
  filter(data %in% substr(seq(as.Date("01/04/2020",format="%d/%m/%y"),
                              as.Date("28/04/2020",format="%d/%m/%y"),
                              by=1),6,10))%>%
  mutate(prop = 100000*(obitosAcumulados/pop_ne))%>%
  group_by(data)%>%
  ggplot(., aes(x = data, y = prop, group = estado, colour = estado)) +
  geom_line(cex=1.1) +
  labs(x = "Data", y = "Mortes por 100 mil habitantes", colour = "Estado",
       title="Figura 5: acumulados dos óbitos por Covid-19 por estado",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.5,segment.colour = "transparent")

16/45
##########################################################################################

url2 <- "https://covid.lais.ufrn.br/dados_abertos/comorbidade_obitos.csv"
dados3 <- read.csv(url2,header=TRUE,sep=";")

x <- unique(dados3$comorbidade)
y <- rep(NA,length(x))
dados <- data.frame(x,y)
for(i in x){
  dados[dados$x==i,2]<-max(dados3[dados3$comorbidade==i,]$total_acumulado)
}
dados[,1]<-as.character(dados[,1])
dados[c(5,8,9,11,12,13,14,15,16,17,18,20,21),1]<-c("DEPENDÊNCIA QUÍMICA","HIPERTENSÃO",
                                                   "PROBLEMAS NEUROLÓGICOS CONGÊNITOS",
                                                   "PROBLEMAS CARDIOLÓGICOS",
                                                   "PROBLEMAS CARDIOLÓGICOS CRÔNICOS",
                                                   "COMPLICAÇÕES CARDÍACAS","DOENÇA RENAL CRÔNICA",
                                                   "FEBRE REUMÁTICA","PNEUMONIA CRÔNICA",
                                                   "SÍNDROME DE DOWN","NEOPLASIA COM METÁSTASE",
                                                   "FRATURA DE FÊMUR","PNEUMONIA DE REPETIÇÃO"
)

ggplot(dados, aes(x=reorder(x,desc(y)), y=y)) + geom_col()+coord_flip()+
  ylab("Quantidade")+xlab(" ")+ 
  labs(title="Figura 5: Condições pré-existentes nos óbitos por \n Covid-19 no RN",
       caption="Fonte: LAIS/HUOL/UFRN    Autor: Thiago Valentim",
       fill="Data")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 9),
        axis.text.x = element_text(vjust=0.6, size = 9,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  scale_y_continuous(breaks = 1:15)

#######################################################################

# Dados LAIS/UFRN

url <- "https://covid.lais.ufrn.br/dados_abertos/faixa_etaria_pacientes_obitos.csv"

coronaRN <- read.csv(url,header=TRUE,sep=";")

faixa<-as.factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                   "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                   "75-79","80-84","85-89","90-94"))

levels(faixa)<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                 "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                 "75-79","80-84","85-89","90-94")

str(coronaRN)

coronaRN$fx_etaria <- factor(coronaRN$fx_etaria,levels=levels(faixa))

coronaRN[46:49,3]<-c("10-14","15-19","25-29","5-9")
coronaRN[46:49,1]<-rep(coronaRN[45,1],4)
coronaRN[46:49,2]<-rep(coronaRN[45,2],4)
coronaRN[46:49,4]<-rep(coronaRN[45,4],4)-1
coronaRN[46:49,5]<-rep(coronaRN[45,5],4)
coronaRN[50:53,3]<-c("10-14","15-19","25-29","5-9")
coronaRN[50:53,1]<-rep(coronaRN[45,1],4)
coronaRN[50:53,2]<-rep(coronaRN[44,2],4)
coronaRN[50:53,4]<-rep(coronaRN[45,4],4)-1
coronaRN[50:53,5]<-rep(coronaRN[45,5],4)


ggplot(data = coronaRN,aes(x = total, y = fx_etaria))+
  geom_bar(aes(x = total, y = fx_etaria),stat = "identity")+coord_flip()+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.5, size = 10),
        axis.text.x = element_text(vjust=0.5, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  labs(x = "Quantidade de óbitos", y = "Faixa etária", 
       fill="Sexo",
       title="Figura 6: Faixa etária dos óbitos por Covid-19 no RN",
       caption="Fonte: LAIS/HUOL/UFRN        Autor: Thiago Valentim")

coronaRN[coronaRN$genero=="Feminino",]$total<- -coronaRN[coronaRN$genero=="Feminino",]$total

ggplot(data = coronaRN,aes(x = total, y = fx_etaria, fill = genero,group = genero))+
  geom_bar(data = subset(coronaRN, genero == "Masculino"), stat = "identity")+
  geom_bar(data = subset(coronaRN, genero == "Feminino"), stat = "identity")+
  scale_x_continuous(breaks = -4:4,limits = c(-4, 4),
                     labels=c("4","3","2","1","0","1","2","3","4"))+
  theme(legend.position="bottom", 
        axis.text.y = element_text(vjust=0.5, size = 10),
        axis.text.x = element_text(vjust=0.5, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  labs(x = "Quantidade de óbitos", y = "Faixa etária", 
       fill="Sexo",
       title="Figura 8: Faixa etária dos óbitos por Covid-19 no RN",
       caption="Fonte: LAIS/HUOL/UFRN        Autor: Thiago Valentim")+
  geom_vline(xintercept=0,linetype="dashed", color = "black")

##############################################################################
