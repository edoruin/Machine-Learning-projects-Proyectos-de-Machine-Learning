#installar librerias:

install.packages('tidyverse')
install.packages('caret')
install.packages('ggplot2')

#Primero, crearemos un dataframe con los datos de empleados:

# Cargar paquetes necesarios
library(tidyverse)
library(caret) # Para particionar los datos y evaluar el modelo


# Creo un data frame 
empleados <- tibble(
  nombre = c("Ana", "Luis", "Marta", "Pedro", "Sofía", "Carlos", "María", "Jorge", "Laura", "Fernando"),
  edad = c(23, 30, 22, 34, 28, 45, 33, 29, 31, 40),
  experiencia = c(1, 8, 2, 10, 5, 20, 12, 7, 9, 15),
  ciudad = c("Madrid", "Barcelona", "Valencia", "Sevilla", "Bilbao", "Madrid", "Barcelona", "Valencia",
             "Sevilla", "Bilbao"),
  salario = c(30000, 35000, 32000, 40000, 36000, 45000, 38000, 34000, 37000, 42000))


colSums(is.na(empleados)) #sin valores nulos


#preparacion de datos 

empleados$ciudad <- as.factor(empleados$ciudad) 
empleados$ciudad <- as.numeric(empleados$ciudad)#convierto a factor la columna ciudad y luego a tipo numerico para poder utilizar la funcion cor()

str(empleados$ciudad)


#particionamos los datos 

set.seed(2)


# create training and test sets
inTrain <- caret::createDataPartition(y = empleados$salario, p = 0.5, list = FALSE)

# Crear conjuntos de entrenamiento y prueba
trainData <- empleados[inTrain, ]
testData <- empleados[-inTrain, ]

#seleccionando variables 

names(empleados)
#hay una correlacion alta entre experiencia y salario, salario y edad y experiencia y edad
# ..por otro lado ciudad no  demuestra relacionarse con ninguna de las demas variables

pairs(empleados[,c('experiencia','salario')]) 
pairs(empleados[,c('edad','salario')]) 
pairs(empleados[,c('experiencia','edad')]) 
pairs(empleados[,c('ciudad','salario')]) 
pairs(empleados[,c('experiencia','ciudad')]) 
pairs(empleados[,c('ciudad','edad')])  #se consigue encontrar una correlacion cuando se presenta una linea ascendente entre las variables

#correr las lineas de arriba una por una


cor(empleados[,c('edad','experiencia','salario','ciudad')])  #tambien puedo mostrar las relaciones con una tabla con la funcion cor()
#de nuevo demuestra que ciudad no tiene  una relacion con las demas variables
#una correlacion en 0 es una correlacion nula


#RESUMEN:
cat('las variables de experiencia y edad en esta funcion muestran una alta correlacion con el salario','\n','\n',
'la variable ciudad muestra una nula correlacion')


# entrenando el modelo

regresion <- lm(salario~ experiencia + edad ,data = trainData) #utilizamos las variables seleccionadas para crear el modelo




#haciendo predicciones 
predicciones <- predict(regresion,testData) #haciendo las predicciones 


#se puede comparar de la siguiente manera:

cat(paste('\n','\n','prediccion: ', '\n',round(predicciones),'\n', 'realidad: ', '\n', testData$salario))




#Grafico de dispersion de los resultados:

ggplot(testData, aes(x = experiencia, y = salario)) +
  geom_point(color = 'red') +  
  geom_line(aes(y = predicciones), color = "blue") +  # línea de ajuste
  labs(x = "prediccion", y = "realidad") +
  theme_minimal()


#RMSE

errores <- predicciones - testData$salario
RMSE <- sqrt(mean(errores^2))
print(paste('RMSE: ',RMSE))

#R^2
summary(regresion) #Multiple R-squared:  0.9618, Adjusted R-squared:  0.9223
#:,D

R2 <- cor(testData$salario, predicciones)^2
print(paste('R2: ', R2))

#estos resultados en los comentarios podrian cambiar al momento de separar la data,
# pero no seran muy diferentes de esos




# Analiza los coeficientes del modelo y las métricas de rendimiento para entender cómo
# cada variable afecta al salario y la precisión del modelo.



coeficientes <- coef(regresion)
coeficientes
#El analisis de los coeficientes es algo incomodo, puesto que las variables independientes usadas estan 
#fuertemente relacionadas entre si, es posible que se haya generado multicolinealidad, esto es que el modelo
#no pueda dividir los coeficientes con exactitud por los similares que son las variables..lo cual elimina la precision de los coeficientes
#al momento de analizar el modelo. 
# esto se puede confirmar al momento de revisar el resumen del modelo, con el error estandar que demuestra la diferencia de los coeficientes con la realidad

#El RMSE demuestra valores entre los 1100 los cuales representan la diferencia entre las predicciones y la realidad
#el R^2 esta entre el 88 y el 95 porciento 


#Volviendo a los coeficientes, estos muestran que cuando la experiencia se mantiene en constancia, el salario aumenta.
# Y en el  caso de edad, aumenta aun mas el salario 

#el coeficiente intercept representa el valor que predice el modelo cuando 
# las variables son igual a 0

#no agregue la variable ciudad pero estoy casi seguro de que daria 0, por su nula relacion

