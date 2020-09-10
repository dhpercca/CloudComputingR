library(readxl)
dataset <- read_excel("Crickes_Chrips_Temperture_RL.xlsx")
summary(dataset)
head(dataset)
tail(dataset)

##Gráfico de dispersión

plot(dataset$`Chirps/15s`, dataset$`Temp(C)`)


##Construyendo una matriz de covarianza
cov(dataset[,-(1:2)])


##Calculando correlaciones

cor(dataset[,-(1:2)])
# cor(dataset[,c(3:6)])


##Calculando el modelo

modelo = lm(dataset$`Temp(C)`~ dataset$`Chirps/15s`)
# ln (v. dependiente ~ v. independiente(s))


##Resultados del modelo

summary(modelo)


##Graficando la linea de Regresión

plot(dataset$`Chirps/15s`, dataset$`Temp(C)`)
abline(modelo, col = "red")
