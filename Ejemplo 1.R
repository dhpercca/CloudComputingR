knitr::opts_chunk$set(echo = TRUE)

# Ejemplo 1
#Análisis de covarianza y Correlación
##Lectrura de los Datos

mydata=read.csv("https://goo.gl/SsfWgg")


#Verificar estructura de los datos leidos

str(mydata)

head(mydata)

tail(mydata)


##Visualización de los datos

#head(mydata)
#attach(mydata)
plot(mydata$payroll,mydata$wins,main= "Grafico de Dispersión", col = "red")


##Calculando la covarianza (matriz)

cov(mydata[,-1])



##Calculando la correlación

cor(mydata[,-1])
cor(mydata$payroll,mydata$wins) 

#cor(mydata[,2],mydata[,3])

