#Analisis de intensidad de fluorescencia
#Cruce insc-GAL4>UAS-CUTie2 25 vs 29 grados
#Ana Clara Gonzalez; anaclgonzalez92@gmail.com
#25 de Noviembre de 2021
#
#Cargo los archivos de cada carpeta en las variables correspondientes
archivos25 = list.files("D:/Maestria neurodesarrollo/fotos y resultados ensayos/Microscopìa confocal/23112021 insc x CUTie2 25 vs 29/nuevo calculo de intensidad de z stack elegidos/25/", all.files = F, full.names = TRUE)
archivos29 = list.files("D:/Maestria neurodesarrollo/fotos y resultados ensayos/Microscopìa confocal/23112021 insc x CUTie2 25 vs 29/nuevo calculo de intensidad de z stack elegidos/29/", all.files = F, full.names = TRUE)
#Llamo los archvos de 25
datos25_1 <- read.csv(archivos25[1])
datos25_2 <- read.csv(archivos25[2])
datos25_3 <- read.csv(archivos25[3])
datos25_4 <- read.csv(archivos25[4])
datos25_5 <- read.csv(archivos25[5])
datos25_6 <- read.csv(archivos25[6])
datos25_7 <- read.csv(archivos25[7])
datos25_8 <- read.csv(archivos25[8])
datos25_9 <- read.csv(archivos25[9])
datos25_10 <- read.csv(archivos25[10])
#
#test de Normalidad de Shapiro-wilks para ver si uso la media o la mediana para los graficos
shapiro.test(datos25_1[,3])
shapiro.test(datos25_2[,3])
shapiro.test(datos25_3[,3])
shapiro.test(datos25_4[,3])
shapiro.test(datos25_5[,3])
shapiro.test(datos25_6[,3])
shapiro.test(datos25_7[,3])
shapiro.test(datos25_8[,3])
shapiro.test(datos25_9[,3])
shapiro.test(datos25_10[,3])
#
#Agrupo los datos en CFP e YFP
datos25CFP <- cbind.data.frame(datos25_1[1:10,3],  datos25_3[1:10,3],  datos25_5[1:10,3],  datos25_7[1:10,3],  datos25_9[1:10,3] )
datos25YFP <- cbind.data.frame(datos25_2[1:10,3], datos25_4[1:10,3], datos25_6[1:10,3], datos25_8[1:10,3], datos25_10[1:10,3] )
colnames(datos25CFP) <- c("cerebro 1","cerebro 2", "cerebro 3", "cerebro 4", "cerebro 5")
colnames(datos25YFP) <- c("cerebro 1","cerebro 2", "cerebro 3", "cerebro 4", "cerebro 5")
#Agrupo todo eso en una tabla de CFP e YFP
datos25group <- cbind.data.frame(datos25CFP, datos25YFP)
colnames(datos25group) <- c("CFP", "YFP")
#SAco la mediana de cada uno y armo una tabla con eso para graficar
medianas25CFP <- c(median(datos25_1[,3]), median(datos25_3[,3]), median(datos25_5[,3]), median(datos25_7[,3]), median(datos25_9[,3]))
medianas25YFP <- c(median(datos25_2[,3]), median(datos25_4[,3]), median(datos25_6[,3]), median(datos25_8[,3]), median(datos25_10[,3]))
medianas25 <- cbind(medianas25CFP, medianas25YFP)
colnames(medianas25) <- c("CFP", "YFP")
#
#Llamo los archivos de 29
datos29_1 <- read.csv(archivos29[1])
datos29_2 <- read.csv(archivos29[2])
datos29_3 <- read.csv(archivos29[3])
datos29_4 <- read.csv(archivos29[4])
datos29_5 <- read.csv(archivos29[5])
datos29_6 <- read.csv(archivos29[6])
datos29_7 <- read.csv(archivos29[7])
datos29_8 <- read.csv(archivos29[8])
datos29_9 <- read.csv(archivos29[9])
datos29_10 <- read.csv(archivos29[10])
#
#test de Normalidad de Shapiro-wilks para ver si uso la media o la mediana para los graficos
shapiro.test(datos29_1[,3])
shapiro.test(datos29_2[,3])
shapiro.test(datos29_3[,3])
shapiro.test(datos29_4[,3])
shapiro.test(datos29_5[,3])
shapiro.test(datos29_6[,3])
shapiro.test(datos29_7[,3])
shapiro.test(datos29_8[,3])
shapiro.test(datos29_9[,3])
shapiro.test(datos29_10[,3])
#
#Agrupo los datos de la columna de fluorescencia en CFP e YFP
datos29CFP <- cbind.data.frame(datos29_1[1:10,3],  datos29_3[1:10,3],  datos29_5[1:10,3],  datos29_7[1:10,3],  datos29_9[1:10,3] )
datos29YFP <- cbind.data.frame(datos29_2[1:10,3], datos29_4[1:10,3], datos29_6[1:10,3], datos29_8[1:10,3], datos29_10[1:10,3] )
colnames(datos29CFP) <- c("cerebro 1","cerebro 2", "cerebro 3", "cerebro 4", "cerebro 5")
colnames(datos29YFP) <- c("cerebro 1","cerebro 2", "cerebro 3", "cerebro 4", "cerebro 5")
#Agrupo todo eso en una tabla de CFP e YFP
datos29group <- cbind(datos29CFP, datos29YFP)
colnames(datos29group) <- c("CFP", "YFP")
#SAco la mediana de cada uno y armo una tabla con eso para graficar
medianas29CFP <- c(median(datos29_1[,3]), median(datos29_3[,3]), median(datos29_5[,3]), median(datos29_7[,3]), median(datos29_9[,3]))
medianas29YFP <- c(median(datos29_2[,3]), median(datos29_4[,3]), median(datos29_6[,3]), median(datos29_8[,3]), median(datos29_10[,3]))
medianas29 <- cbind(medianas29CFP, medianas29YFP)
colnames(medianas29) <- c("CFP", "YFP")
#Grafico de box-plot basico
boxplot(dF)
#Agrego una columna con los valores de temperatura para poder unificar
temp <- c(25,25,25,25,25)
medianas25 <- cbind(medianas25, temp)
temp <- c(29,29,29,29,29)
medianas29 <- cbind(medianas29, temp)
#Hago un dF con los datos de 25 y 29 juntos
dF <- rbind.data.frame(medianas25, medianas29)
#colnames(dF) <- c("25", "29")
#Indico que comparar
comp <- list(c("25", "29"))
#
#-----------Chunk agregado por Daniel el 25/11/2021---------------
#armo la tabla con los datos crudos sin usar la mediana
CFP25 <-  c(datos25_1[,3], datos25_3[,3], datos25_5[,3], datos25_7[,3], datos25_9[,3])
YFP25 <- c(datos25_2[,3], datos25_4[,3], datos25_6[,3], datos25_8[,3], datos25_10[,3])
CFP29 <-  c(datos29_1[,3], datos29_3[,3], datos29_5[,3], datos29_7[,3], datos29_9[,3])
YFP29 <- c(datos29_2[,3], datos29_4[,3], datos29_6[,3], datos29_8[,3], datos29_10[,3])
#
#agrupo los datos en un DF y nombro las columnas
d25 <- cbind(CFP25, YFP25, 25)# agrego una tercer columna que estarÃ¡ llena de 25s
colnames(d25) <- c("CFP","YFP", "temp") 
d29 <- cbind(CFP29, YFP29, 29)# agrego una tercer columna que estarÃ¡ llena de 29s
colnames(d29) <- c("CFP","YFP", "temp") 
dFgraph <- rbind.data.frame(d25, d29)#para que funcione las columnas deben tener los mismos nombres
#
#Actualice los graficos para que muestren estos datos sin esconderlos detras de las medianas.
#Si quieres que muestre el p-valor exacto usa map_signif_level=FALSE
#END of---------Chunk agregado por Daniel el 25/11/2021---------------



#GRAFICO
library(reshape2)
#melt(dF, id.vars = 1, measure = 2:ncol(dF)-1)
library(ggplot2)
library(ggpubr)#La biblioteca que nos permite agregar la capa de estadistica
p1 <- ggplot(dFgraph, aes(y=CFP, factor(temp)))+
  geom_boxplot(size=1.6)+
  geom_jitter(width = 0.1, alpha=0.5)+#esta va de yapa por si te gusta
  theme(axis.title.x=element_text(size=14), axis.title.y=element_text(size=18), axis.text.x = element_text(size=14))+
  xlab("Temperatura (°C)")+
  ylim(c(3000,18000))+
  geom_signif(comparisons = comp, test = "t.test", map_signif_level=TRUE, color="black", size = 0.2, textsize = 5)
 
 
#
p2 <- ggplot(dFgraph, aes(y=YFP, factor(temp)))+
  geom_boxplot(size=1.6)+
  xlab("Temperatura (°C)")+
  geom_jitter(width = 0.1, alpha=0.5)+#esta va de yapa por si te gusta
  theme(axis.title.x=element_text(size=14), axis.title.y=element_text(size=18), axis.text.x = element_text(size=14))+
  xlab("Temperatura (°C)")+
  ylim(c(3000,18000))+
  geom_signif(comparisons = comp, test = "t.test", map_signif_level=TRUE, color="black", size = 0.2, textsize = 5)

#
#library(grid) #Ya no la necesitamos porque esta contenida en ggpubr
plot <- ggarrange(p1, p2)#arrangeGrob(p1, p2, ncol=2))
 # ggtitle("Intensidad de fluorescencia por pixel", subtitle = "CUTie2 - 25 vs. 29 °C")+
  annotate_figure(plot, top = text_grob("", 
                                        color = "black", face = "bold", size = 14), fig.lab = "Intensidad de fluorescencia por pixel", fig.lab.face = "bold", fig.lab.size=14)

