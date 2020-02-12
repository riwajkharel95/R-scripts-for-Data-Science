library(tidyverse)

df<-as.data.frame(`20191009_OBETA_DATA_Version_002`)

#Basic cleaning for each column
#Remove whitespaces 

df$Artikelnummer<-gsub('\\s+', '', df$Artikelnummer)
df$Artikelgruppenbezeichnung<-gsub('\\s+', '', df$Artikelgruppenbezeichnung)
df$Vertreter<-gsub('\\s+', '', df$Vertreter)

df$Menge <- as.numeric(gsub(",", ".",df$Menge))
df$Listenpreis.pro.STK <- as.numeric(gsub(",", ".",df$Listenpreis.pro.STK))
df$Netto.Umsatz <- as.numeric(gsub(",", ".",df$Netto.Umsatz))
df$Rabbat.in.EUR <- as.numeric(gsub(",", ".",df$Rabbat.in.EUR))

#Changing date format
df$STDTER <- substr(df$STDTER,2,7)
df$STDTER <- as.Date(df$STDTER,"%y%m%d")
df$STDTER <- format(df$STDTER, "%d.%m.%Y")

#New discount column=List price*Amount-Netsales

df$Rabbat<-(df[,8]*df[,9])-df[,10]

df<-df %>% select(Kundennummer,Auftragsart,Auftragsnummer,Artikelnummer,Artikelbezeichnung,Artikelgruppe,Artikelgruppenbezeichnung,Menge,Listenpreis.pro.STK,Netto.Umsatz,Rabbat,Filialzuordnung,Vertreter,STDTER)


df$Rabbat<-(df[,8]*df[,9])-df[,10]

Omenge<-(df[,8]-mean(df[,8]))/sd(df[,8])
Omenge<- Omenge>3 || Omenge< -3

Olist<-(df[,9]-mean(df[,9]))/sd(df[,9])
Olist<- 3 < Olist< -3

Onet<-(df[,10]-mean(df[,10]))/sd(df[,10])
Onet<-Onet>3 && Onet< -3

df$Outlier_of_Menge <- df$Outlier_of_Listprice>3 | df$Outlier_of_Menge< (-3)


View(df)
df<-df %>% select(Kundennummer,Auftragsart,Auftragsnummer,Artikelnummer,Artikelbezeichnung,Artikelgruppe,Artikelgruppenbezeichnung,Menge,Outlier_of_Menge,Listenpreis.pro.STK,Outlier_of_Listprice,Netto.Umsatz,Outlier_of_Netsales,Rabbat,Filialzuordnung,Vertreter,STDTER)

#Histogram of Amount with Outliers
ggplot(df,aes(df$Menge)) + geom_histogram(binwidth = 800,color="black", fill="red")+labs(x="Amount",title="Histogram of Amount With Outliers")+xlim(-8000,8000)+geom_vline(xintercept = 752,linetype="dashed",color="blue")+geom_vline(xintercept = -752,linetype="dashed",color="blue")+geom_vline(xintercept = mean(df$Menge),linetype="dashed",color="black")+theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                

#Histogram of Smount within 3sd
ggplot(df,aes(df$Menge)) + geom_histogram(binwidth = 100,color="black", fill="red")+labs(x="Amount",title="Histogram of Amount within 3sd")+xlim(-800,800)+geom_vline(xintercept = 752,linetype="dashed",color="blue")+geom_vline(xintercept = -752,linetype="dashed",color="blue")+geom_vline(xintercept = mean(df$Menge),linetype="dashed",color="black")+theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
                                                                                                                                                                                                                                                                                                                                 
#Histogram of List with Outliers
ggplot(df,aes(df$Listenpreis.pro.STK)) + geom_histogram(binwidth = 100,color="black", fill="red")+labs(x="ListPrice",title="Histogram of Listprice With Outliers")+geom_vline(xintercept = 1008,linetype="dashed",color="blue")+geom_vline(xintercept = -1008,linetype="dashed",color="blue")+geom_vline(xintercept = mean(df$Listenpreis.pro.STK),linetype="dashed",color="black")+xlim(0,5000)+ylim(0,2000000)+theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

#Histogram of List within 3sd
ggplot(df,aes(df$Listenpreis.pro.STK)) + geom_histogram(binwidth = 100,color="black", fill="red")+labs(x="ListPrice",title="Histogram of Listprice within 3sd")+geom_vline(xintercept = 1008,linetype="dashed",color="blue")+geom_vline(xintercept = -1008,linetype="dashed",color="blue")+geom_vline(xintercept = mean(df$Listenpreis.pro.STK),linetype="dashed",color="black")+xlim(0,1100)+ylim(0,2000000)+theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

#Histogram of Netsales with outliers
ggplot(df,aes(df$Netto.Umsatz)) + geom_histogram(binwidth = 1000,color="black", fill="red")+xlim(-120000,120000)+labs(x="NetSales",title="Histogram of NetSales")+geom_vline(xintercept = 110071,linetype="dashed",color="blue")+geom_vline(xintercept = -110071,linetype="dashed",color="blue")
