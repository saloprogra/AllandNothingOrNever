#################################################################### 
####            Analizando la Datasets NETFLIX                  ####
####################################################################

# Nota debe primero mantener la carpeta en un espacio o area de trabajo asignado 
# en el menu session, que nos lleva a set working directory y luego choose directory

rm(list = ls())

#################################################################### 
####            Cargando las librerías.                         ####
####################################################################

library(readr) # lee el paquete readr para conectar la datsets.csv
library(dplyr) # para trabajos consultas operador pipe etc.
library(stringr) # libreria para trabajar con cadenas de string
library(DataExplorer) # debe instalar data explorer install.packages("DataExplorer")
#library(Hmisc) # librer.

#################################################################### 
####            Cargando los Datos de la dataset NETFLIX        ####
####################################################################
# la conección se envía una variable datamov que contiene todos la informacion
#datamov <- read_csv("MagisterUdla/TrabajoSalomonSalinasMag_PrograR/n_movies.csv")
datamov <- read_csv("n_movies.csv")
#datamov <- read_csv("MagisterUdla/TrabajoSalomonSalinasMag_PrograR/n_movies.csv")
View(datamov) # se muestra en vista la tabla datamov


############################################################################### 
####            Cargando los Datos de dataset NETFLIX.                     ####
###############################################################################
#muestra los datos por consola show()
show(datamov)
datamov<- data.frame(datamov)

############################################################################### 
####            Limpieza de la Datasets NETFLIX.                           ####
###############################################################################
# se procederá a cambiar algunos detos de información de la dataset,
# ya que cuenta con campos con NA, atributos numéricos que contiene 
# caracteres, y reemplazar los nombres de los campos de la tabla.

#Cambiamos los nombres de los atributos de la tabla datamov
colnames(datamov)<-c("titulo","anio","canal", "dura_min", "genero",
                     "rating", "descripcion", "actores", "votos")

#verificamos los nombres de las nuevos nombres de las columnas
names(datamov)

#Veo que en realidad voy explorar sólo los datos principales como el título, año,
#canal, duración en minutos, genero y votos, pero actores y descripción por el
# momento no se van a solicitar, ya que se presentará información de otra indole
# para ellos procedo a eliminar las columnas que no ocuparé.
datamov<- datamov[,c(1,2,3,4,5,6,9)] # se eliminó la columna descripción y actores
head(datamov,3)

#verificamos los nombres de las nuevos nombres de las columnas si se realizó el 
#cambio
names(datamov)

## usando la tabla apply, cuya funcion se le pasa tres parametros para que sumen los valores faltantes
apply(X = is.na(datamov), MARGIN = 2, FUN = sum)

## usando la tabla sapply cuya funcion igual se le pasa los 3 parametros y nos muestra los valores 
# faltantes por columnas.
sapply(datamov, function(x) sum(is.na(x)))

#determinación del porcentaje de valores perdidos respecto del total de datos
#para cada columna
porcentajeMiss <- function(x) {sum(is.na(x)) / length(x)*100}
# por columna
apply(datamov, 2, porcentajeMiss)

#sumando los valores nulos de la totalidad de la tabla
valnulos<- sum(is.na(datamov))
valnulos

#sumando registros sin valores nulos de la tabla
sumreg<-sum(complete.cases(datamov))
sumreg
# podemos decir que hay mas valores nulos que la cantidad de datos dentro de la tabla

#omitiendo los valores nulos
#omitvalnull<- na.omit(datamov)
#omitvalnull

#buscando NA
is.na.data.frame(datamov)
#valores atipicos buscando
is.na(datamov)

#ubicacion de los NA
which(is.na(datamov$genero))

#2858 3873 4256 4274 5406 5453 5468 5850 6003 6033 6035 6216 6280 6340 6631 6671 6744 6923 6939
#[20] 7030 7043 7100 7101 7111 7236 7290 7292 7313 7417 7459 7470 7512 7526 7542 7545 7549 7594 7612
#[39] 7632 7664 7676 7690 7691 7697 7698 7704 7712 7730 7733 7739 7742 7790 7794 7804 7817 7823 7824
#[58] 7831 7832 7833 7845 7849 7872 7875 7883 7907 7918 7922 7946 7969 7981 7986 8015

#datagen<-datamov$genero
#datagen

#datos_g<- datamov$genero[is.na(datamov$genero)]<-"SinG"
#datos_g

#datos_g<- datamov$genero[is.null(datamov$genero)]<-"SinGenerolok"
#datos_g


#str <- datamov$genero   
#str_match(str, ',\\s(\\w+)')[,2]

which(is.na(datamov$genero))
##promediando los valores en rating y votos para reemplazar los valores nulos
##para ello haré un summary para ver el promedio
summary(datamov)#6.764 es la media de rating y votos 19523

# se asignó un promedio de rating a la dataset un datos a la tabla con valores Singenero
datos_g<- datamov$genero[is.na(datamov$genero)]<-"SinGeneral"
datos_g

datos_c<- datamov$canal[is.na(datamov$canal)]<-"Not Rated"
datos_c

# comprobando si tiene valores nulos
danullg<- sum(is.na(datamov$genero))
danullg

#Muestra una tabla con los valores de los campos con valores na y los suma
apply(X = is.na(datamov), MARGIN = 2, FUN = sum)


# se asignó un promedio de rating a la dataset
datos_t<- datamov$rating[is.na(datamov$rating)]<-6.764
datos_t
# comprobando
valnulos<- sum(is.na(datamov$rating))
valnulos
# asignando al campo vacio la media generada por el resumen summary
datos_b<- datamov$votos[is.na(datamov$votos)]<-19523
datos_b

# comprobando
valnulos<- sum(is.na(datamov$votos))
valnulos
## usando la tabla apply, cuya funcion se le pasa tres parametros para que sumen los valores faltantes
apply(X = is.na(datamov), MARGIN = 2, FUN = sum)

#removiendo min del atributo campo dura_min
#print(str_replace_all(datamov$dura_min, "[^[:alnum:]]", ""))
datamovsinnull<- datamov
datamovsinnull
apply(X = is.na(datamovsinnull), MARGIN = 2, FUN = sum)

#permite la funcion localizar el campo reemplazar el string por valores blancos
anios<-gsub('[() ]*', "", datamov$anio)
anios<-gsub(c("I"), "", anios)
anios<-gsub(c("V"), "", anios)
anios<-gsub('" "*','',anios)
anios<-substr(anios,1,4)
na.omit(anios)



############################################################################### 
####            Identificando los tipos de variables.                      ####
###############################################################################

# Declarando 2 variables Cualitativas. genero, certificate
##conviertiendo a canal en factor
certificado<-factor(datamov$canal) # declarando una variable certificado nueva con factor 

##conviertiendo a genero en factor
generos<-factor(datamov$genero) # declarando una variable nueva generos con factor


# permite la funcion localizar el campo reemplazar el string por valores blancos
duracion<-gsub("min", "", datamov$dura_min)

#str(duracion)  aqui convertimos un string A INTEGER
dura_minutos<-as.integer(duracion) # declarando una variable nueva dura_minutos
#str(dura_minutos)

#anios<-as.numeric(anios) #declarnado una variable nueva como integer anios
anios<-suppressWarnings(as.numeric(anios))  

#creamos una variable datamov que contiene los campos nuevos  con cbind
datamov3<-cbind(datamov,certificado,generos ,dura_minutos,anios) # agrega una columna nueva
datamov3<-data.frame(datamov3)  #lo convierte en dataframe para trabajar en tablas
datamov3 <- datamov3[,-c(2,3,4,5)] #eliminamos los campos que estamos reemplazando por los nuevos certificado,generos,dura_minutos,anios

# la media es 73.77 del campo dura_minutos se redondea a 74
#summary(datamov3)# resumen general para ver la media
datos_dm<- datamov3$dura_minutos[is.na(datamov3$dura_minutos)]<-74 #reemplazamos los nulos por la media 74 en campo dura_minutos

dats_an<- datamov3$anios[is.na(datamov3$anios)] <- 2022
dats_an

############################################################################### 
####                           Exploracion.                                ####
###############################################################################

# Detalle Resumen Analítico.

## usando la tabla apply, cuya funcion se le pasa tres parametros para que sumen los valores faltantes
apply(X = is.na(datamov3), MARGIN = 2, FUN = sum)
## contabilizando los datos únicos en cada columna
datamov3 %>% summarise_all(funs(n_distinct(.)))

# visualizamos con head los primeros 3 campos de la tabla para observar sus contenidos
head(datamov3, 3)

# visualizamos los ultimos 3 filas de datamov3 para ver sus contendos
tail(datamov3, 3)

# podemos usar colnames 
colnames(datamov3)

#podemos usar names(para ver las variables
names(datamov3)

# podemos usar dim para ver su dimension filas por columnas
dim(datamov3) # esta compuesta por 9957 datos y 7 columnas que se dejaron para trabajar

#Se presenta la estructura de la datamov3 con sus variables creadas y su tipo
str(datamov3)


############################################################################### 
####                         Resumen Analítico.                            ####
###############################################################################

#nos sirve para identificar las variables resumen general de la datasets
introduce(datamov3)
# Explora la datasets rows columns discrete_columns continuous_columns all_missing_columns total_missing_values complete_rows
#1 9957       7                3                  4                   0                  632          9325
#total_observations memory_usage
#1              69699      1075336
#Detalle nos indica que tenemos 3 variables discretas 4 columnas continuas, ceros datos perdidos datos total 9325

# se realiza un sumary para verificar y analizar el comportamineto de los datos
summary(datamov3)
#Observación:

#0.- De la columna titulo de dejo para ver a que pelicula corrscponde el genero. es de tipo caracter

#1.- De la columna rating columna numerica nos indica que su dsitribucion es homogenea ya que se acerca la media y mediana al mismo centro.
# la mayor parte se concetra del segundo y tercer cuartil con valores muy alejados primer cuartil y alejasdos en el cuarto.

#2.- De la columna voto variable numérica que no es uniforme esta muy concetrado en la parte superior, tiene muchos datos atipicos.

#3.- De la columna certificado que es una variable factor que posee niveles

#4.- De la columna genberos es una variable factor ya que posee niveles

#5.- De la columna dura_minutos es bastante uniforme, de igual forma en la contracion esta en la parte superior y tenemos algunos outlier.

#6.- Se estableció como una variable numerica para realizar búsquedas y agrupaciones.



summary(datamov3)

##############Algunas estadística que se pueden realizar#######################

#Funciones uqe se piueden aplicar a tablas enteras.

# funciones a tomar en cuenta moda, valor o valores que mas se repiten
moda <- function(x){
  unico <- unique(x)
  unico[which.max(tabulate(match(x, unico)))]
  
}
moda(datamov3$rating)
# en este caso se le indico que buscar en rating la moda lo cual arrojo 6764

# funciones a tomar en cuenta media dentro del campo
media<-function(x){
  sum(x)/length(x)
}
media(datamov3$rating)

# funciones a tomar en cuenta mediana dentro del campo
median(datamov3$rating)

# funciones a tomar en cuenta el mínimo y máximo a buscar dentro del campo
min(datamov3$rating)
max(datamov3$rating)

#Algunas medidas de dispersión com desviación standard
sigma<- function(x){
  n <- length(x)
  desvi <- sqrt(sum((x-mean(x))^2)/n)
  return(desvi)
}
sigma(datamov3$rating)

#Algunas medidas de dispersión con desviacion standard
coef_var<- function(x, na.ram = FALSE){
  sd(x, na.rm = na.ram) / mean(x, na.ram=na.ram)
}
sd(datamov3$rating)

# declarando variables de las tablas 
tablagenero<-table(generos)
tablagenero
str(tablagenero) # muestra la estructura de la tabla

# consultando por los mínimos y máximos dentro de la dataset y alojando los datos en una variable
# minmax
minmax<-datamov3[datamov3$votos==max(datamov3$votos) | datamov3$votos==min(datamov3$votos),]  # buscando los valores minimos y maximos
minmax

# consultadon y filtrando el rating dentro del año 2019, dejando los resultados en
# una variable ra
ra<-datamov3[datamov3$rating >=6.900 & datamov3$anios == 2019, c('rating','anios')]# filtro con condicion
ra

############################################################################### 
####                          Resumen Grafico.                             ####
###############################################################################

# Aqui visulizo un conjunto de datos numéricos con sus rangos respectivos de variables numéricas
# que nos permite ver su distribución entre sus pares de coordenadas y los valores aoutlier.

datamo <- datamov3[,c("rating","votos","dura_minutos","anios")] # selección de los campos en datamov3
#datamo

pairs(datamo,       # Datos
      pch = 19, # Símbolo pch
      col = 4,  # Color
      main = "Comparativa de Distribución ",    # Título
      gap = 0,           # Distancia entre gráficos
      row1attop = FALSE, # Dirección de la diagonal
      labels = colnames(datamo), # Etiquetas
      cex.labels = 0.8,  # Tamaño textos diagonales
      font.labels = 1)   

# plot para ver nuestra tabla como esta compuesta
plot_intro(datamov3)

# plot donde veremos los valores perdidos si existe o no
plot_missing(datamov3)

# correlación de las varibales numéricas 
plot_correlation(datamov3)

# Ploteando la tabla Resumen tablagenero
barplot(tablagenero, col="blue", main="Distribución de Generos por nivel", xlab = "Generos", ylab = "Rangos")

# graficando variables continuas
#tabla resumen para no datos cualitativos
#aqui podriamos scar los intervalos, usando la regla sturger, para ver cuantos intervalos usar
# 
par(mfrow = c(1,2))
hist(datamov3$rating, breaks = "Sturge", plot=TRUE,main = "Histograma Rating s/ breaks",xlab = "Densidad", ylab = "Frecuencia")
hist(datamov3$rating, breaks = 4, main = "Histograma Rating 4 breaks",xlab = "Densidad", ylab = "Frecuencia")

#tabla resumen para no datos cualitativos
#aqui podriamos scar los intervalos, usando la regla sturger, para ver cuantos intervalos usar
#aplicamos funciones para su minimo y maximo

########################
# creamos un histograma con la tabla de frecuencia como eje de corte

hist(datamov3$rating, breaks = "Sturge", plot=TRUE,main = "Histograma Rating",xlab = "Densidad", ylab = "Frecuencia" )
hist(datamov3$rating, breaks = c(1.700, 2.600, 3.700,4.800,5.900,6.600, 7.700,8.800,9.900),main = "Histograma Rating 2")
tabla <- cut(datamov3$rating, breaks = c(1.700, 2.600, 3.700,4.800,5.900,6.600, 7.700,8.800,9.900))
tabla <- table(tabla)
tabla

hist(datamov3$votos, breaks = "Sturge", plot=TRUE,main = "Histograma Votaciones",xlab = "Votaciones", ylab = "Frecuencia" )
tabla1 <- cut(datamov3$votos, breaks = c(300000, 600000, 900000,1200000,1500000,1800000,2100000))
tabla1 <- table(tabla1)
tabla1

par(mfrow = c(1,2))
barplot(tabla)
barplot(tabla1)

# verificamos la media respecto a su mediana si es uniforme en esta caso es muy uniforme en mayor porcentaje se 
# encuentra entre 6 y 8 en su rating pero también se ven muchos valores outlier por sobre su tercer cuartil y por bajo su primer cuartil
par(mfrow = c(1,2))
boxplot(datamov$rating, col ="cyan",main="Rating")
abline(h=mean(datamov3$rating), col="red")
boxplot(datamov3$dura_minutos, col ="blue", main="Duración")
abline(h=mean(datamov3$dura_minutos), col="red")


# la mediana esta bajo la media, se puede identifacar los gráficos, que no es simetrico, tiene datos atípicos.
# segun grafico los datos no tiene un comportamiento simétrico,
# donde el 50 % de los datos se encuentra bajo
# la mayoria de los datos esta dentro de una rango de los 2000 votos
# teniendo valores que sobresalen dentro del tercer cuarto hablamos que su 
# aglomeracion de datos esta dentro de los 

#summary(datamov3$votos)
datamov3$votos
boxplot(datamov3$votos, col ="green")
abline(h=mean(datamov3$votos), col="red")

barplot(datamov3$votos)

#attach(datamov3)
mi_tabla <- table(datamov3$rating)
mi_tabla

# Una fila, dos columnas
par(mfrow = c(1, 2))
# Gráfico de barras de frecuencia absoluta
barplot(mi_tabla, main = "Frequencia absoluta",col = rainbow(3))
# Gráfico de barras de frecuencia relativa
barplot(prop.table(mi_tabla) * 100, main = "Frequencia relativa (%)",col = rainbow(3))

par(mfrow = c(1, 1))
plot(factor(datamov$canal), col = rainbow(3))

# Coma como separador y punto como separador decimal
write.csv(datamov3, "datamovi.csv")

