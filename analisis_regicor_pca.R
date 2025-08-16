## TRABAJO REGICOR
## MODELIZACIÓN ESTADÍSTICA DE DATOS DE ALTA DIMENSIÓN

## Pedro Pérez Francos
## pedro.perez2@udc.es

## Nicolás Aller Ponte
## nicolas.aller@udc.es


# EJERCICIO 1: Realizar un estudio descriptivo de las variables que se 
#consideren de interés
attach(regicor)
head(regicor)

# Nuestros datos contienen información sobre las características fisiológicas
# de un conjunto de personas y la consecuente relación sobre si han tenido un 
# fallo cardiovascular o no.

# Resumen de la base de datos.
summary(regicor)

# En nuestro conjunto de datos podemos situar 21 variables diferentes, de las
# cuales podemos observar que 6 de ellas son cualitativas(sex, smoker...) y
# las 15 restantes son numéricas (age, sbp, dbp...)

# Para proceder al análisis eliminamos las variables cualitativas
datos <- regicor[, -c(2,3,6,7,12,13)]

#Resumen de datos en función del fallo cardiovascular
library(RcmdrMisc)

numSummary(datos,groups = cv)

# Observamos la matriz de varianzas-covarianzas de las variables numéricas
cov(datos)

# Observamos la matriz de correlaciones de las variables numéricas
cor(datos)

##meter todos los histogramas en funcion de si hay fallo cardiovasc o no
par(mfrow=c(2,8))

hist(regicor[cv==0,]$age, main="Edad de la persona", xlab = "Edad")
hist(regicor[cv==1,]$age, add = TRUE, col = rgb(0, 1, 0))

hist(regicor[cv==0,]$sbp, main="Presión arterial sistólica", xlab = "SBP")
hist(regicor[cv==1,]$sbp, add = TRUE, col = rgb(0, 1, 0))

hist(regicor[cv==0,]$dbp, main="Presión arterial diastólica", xlab = "DBP")
hist(regicor[cv==1,]$dbp,add = TRUE, col = rgb(0, 1, 0))

hist(regicor[cv==0,]$chol, main="Colesterol total", xlab = "Colesterol")
hist(regicor[cv==1,]$chol, add = TRUE, col = rgb(0, 1, 0))

hist(regicor[cv==0,]$hdl, main="HDL colesterol", xlab = "HDL")
hist(regicor[cv==1,]$hdl, add = TRUE, col = rgb(0, 1, 0))

hist(regicor[cv==0,]$triglyc, main="Triglicéridos", xlab = "Triglicéridos")
hist(regicor[cv==1,]$triglyc, add = TRUE, col = rgb(0, 1, 0))

hist(regicor[cv==0,]$ldl, main="LDL colesterol", xlab = "LDL")
hist(regicor[cv==1,]$ldl, add = TRUE, col = rgb(0, 1, 0))

hist(regicor[cv==0,]$height, main="Altura", xlab = "Altura de la persona")
hist(regicor[cv==1,]$height, add = TRUE, col = rgb(0, 1, 0))

hist(regicor[cv==0,]$weight, main="Peso de la persona", xlab = "Peso")
hist(regicor[cv==1,]$weight, add = TRUE, col = rgb(0, 1, 0))

hist(regicor[cv==0,]$bmi, main="Índice de masa corporal", xlab = "IMC")
hist(regicor[cv==1,]$bmi, add = TRUE, col = rgb(0, 1, 0))

hist(regicor[cv==0,]$phyact, main="Actividad física", xlab = "Act. física")
hist(regicor[cv==1,]$phyact, add = TRUE, col = rgb(0, 1, 0))

hist(regicor[cv==0,]$pcs, main="Resumen de componente física", 
     xlab = "Componente física")
hist(regicor[cv==1,]$pcs, add = TRUE, col = rgb(0, 1, 0))

hist(regicor[cv==0,]$mcs, main="Resumen de componente mental",
     xlab = "Componente mental")
hist(regicor[cv==1,]$mcs, add = TRUE, col = rgb(0, 1, 0))

hist(regicor[cv==0,]$tocv, main="Tiempo hasta fallo cardiovascular",
     xlab = "Tiempo")
hist(regicor[cv==1,]$tocv, add = TRUE, col = rgb(0, 1, 0))

par(mfrow=c(1,1))
boxplot(regicor$tocv ~ regicor$cv, data = data.frame(regicor), 
        main="Boxplot de Tiempo hasta fallo cardiovascular por Fallo cardiovascular", 
        xlab="Fallo cardiovascular (0 = No, 1 = Sí)", 
        ylab="Tiempo hasta fallo cardiovascular", col=c("lightblue", "lightpink"))

#EJERCICIO 2
# Necesitamos comprobar la existencia o no de multicolinealidad, en el caso
# de que las variables estén muy correladas podríamos disminuir 
# considerablemente su dimensión, en caso de que fueran incorreladas
# no tendría sentido realizar un análisis de componentes principales.

# Matriz de correlaciones
cor(datos)

# Podemos apreciar que las correlaciones entre las variables no superan el |0.5|,
# excepto entre el peso y el bmi
# de todas formas, puede que exista una relación perfecta entre una variable y
# el resto y sus coeficientes de correlación sean bajos, por lo tanto, usaremos
# el siguiente método

# Índice de Kaiser-Meyer-Olkin
library(psych)
KMO(datos)
# El índice es 0.5, por lo que asumiremos que aplicar un ACP sería eficaz

datos_acp <- princomp(datos, cor = TRUE)
summary(datos_acp)
# Para superar el 85% de la variabilidad total se deberían escoger un total de
# 9 componentes principales, pero, debido a que representar en tales dimensiones
# resulta imposible, nos quedaremos tan solo con 2 componentes principales,
# representando tan solo el 33.6% de la variabilidad total

screeplot(datos_acp, type = 'lines', npcs=15)


#EJERCICIO 3: ¿Cómo puede medirse el grado de relación de los nuevos factores 
# obtenidos y las variables originales? ¿Cuál es el valor de esa medida de 
# relación para los factores de las dos primeras dimensiones encontradas?
# Dar una interpretación para esos dos primeros factores. Representar 
# gráficamente los datos de la muestra en el nuevo espacio de dimensión reducida.

datos_acp$loading[,1:9]
datos_acp$loading[,1:2]

plot(datos_acp$scores[,1:2])



#EJERCICIO 4: Dar las fórmulas que permiten calcular las nuevas dimensiones 
# en términos de las variables originales.

datos_acp$loadings[,1:2]

#EJERCICIO 5: ¿Qué proporción de información es capaz de explicar cada factor
# del nuevo espacio?

summary(datos_acp)


#EJERCICIO 6: En el nuevo espacio de dimensión reducida, 
# ¿qué variable original queda mejor explicada? ¿Por qué?
datos_acp$loadings[, 1:9]
rowSums(abs(datos_acp$loadings[, 1:9]))

# La variable que queda mejor explicada es phyact (actividad física)

#EJERCICIO 7: Construir dos representaciones gráficas que representen la 
# importancia relativa de los factores encontrados

# Obtener los valores propios
valores_propios <- datos_acp$sdev^2

# Calcular la proporción de varianza explicada por cada componente principal
proporcion_explicada <- valores_propios / sum(valores_propios)

#Gráfico de varianza explicada (POSIBLE CAMBIO)
cum_proporcion_explicada <- cumsum(proporcion_explicada)
plot(cum_proporcion_explicada, type = "o", 
     xlab = "Número de Componente Principal",
     ylab = "Proporción Acumulativa de Varianza Explicada")
abline(h = 0.85, col = "red", lty = 2)  
# Línea para indicar un umbral del 85% de varianza explicada


screeplot(datos_acp, type="lines", npcs = 15)

#EJERCICIO 8: ¿Qué variabilidad tienen los nuevos factores obtenidos?

(datos_acp$sdev^2)[1:9]


#EJERCICIO 9: Obtener la matriz con las coordenadas de cada dato de la muestra
#en el nuevo espacio y representar los datos sobre el nuevo espacio.

matriz_coordenadas <- datos_acp$scores[,1:2]
plot(matriz_coordenadas, ylim = c(-5, 5))
head(matriz_coordenadas)


#EJERCICIO 10: Construir un gráfico que permita visualizar no solo los datos 
# en el nuevo espacio, sino también las relaciones de las variables originales 
# con las nuevas dimensiones
par(mfrow=c(1,1))
biplot(datos_acp)

#EJERCICIO 11: ¿Resultaría conveniente modificar la forma de obtener los nuevos 
# factores (de dimensión reducida) para mejorar su interpretabilidad?
# ¿De qué forma podría hacerse?

#Se podría hacer una rotación varimax para mejorar la visión e interpretación
# para el posterior análisis de las variables
varimax(loadings(datos_acp)[,1:9])

#EJERCICIO 12:

datos_mujer <- regicor[sex=="Female",]
datos_hombre <- regicor[sex=="Male",]
par(mfrow=c(1,1))

datos_hombre_acp <- princomp(datos_hombre[,-c(2,3,6,7,12,13)], cor = T)  
barplot(loadings(datos_hombre_acp)[,1:9],beside=T, 
        main = "Sexo: Hombre")
datos_mujer_acp <- princomp(datos_mujer[,-c(2,3,6,7,12,13)], cor=T)
barplot(loadings(datos_mujer_acp)[,1:9],beside=T, 
        main = "Sexo: Mujer")


datos_fumador <- regicor[smoker == "Former >= 1y",]
datos_fumador_no_habitual <- regicor[smoker == "Current or former < 1y",]
datos_no_fumador <- regicor[smoker == "Never smoker",]

datos_fumador_acp <- princomp(datos_fumador[, -c(2,3,6,7,12,13)], cor = T)
datos_fumador_no_habitual_acp <- princomp(datos_fumador_no_habitual
                                          [, -c(2,3,6,7,12,13)], cor = T)
datos_no_fumador_acp<- princomp(datos_no_fumador[, -c(2,3,6,7,12,13)], cor = T)


barplot(loadings(datos_fumador_acp)[,1:9], beside = T,
                 main = "Fumador")
barplot(loadings(datos_fumador_no_habitual_acp)[,1:9], beside = T,
        main = "Fumador no habitual")
barplot(loadings(datos_no_fumador_acp)[,1:9], beside = T,
        main = "No fumador")

