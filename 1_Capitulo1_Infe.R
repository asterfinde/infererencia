
######################################################
## Curso: Inferencia Estadística                    ##
## Profesor: Clodomiro Fernando Miranda Villagómez  ##
######################################################

###################################
## Simulación con v.as discretas ##
###################################

ale=c()
p1<-function(r,fx)
{Fx=cumsum(fx)
Fx1=c(0,Fx)
for(i in 1:length(x))
{
  if((Fx1[i]<=r)&(r<Fx1[i+1]))
  {ale=c(ale,x[i])
  return(ale)}
}
}

resul1=c()
rV<-function(rn)
{
  for(i in 1:length(rn))
  {
    resul=p1(rn[i],fx)
    resul1=c(resul1,resul)
  }
  return(resul1)
}

## Lo que se pide en la guia
x=-2:2
fx=c(0.1,0.2,0.2,0.3,0.2)
random1=c(0.081,0.1,0.35,0.82,0.99)
rsimulada=rV(random1)
rsimulada  

## simulacion de una m.a de tamano 20
x=-2:2
fx=c(0.1,0.2,0.2,0.3,0.2)

ran=20
set.seed(2021)
random1=runif(ran)
rsimulada=rV(random1)
rsimulada

## El caso de una Binomial(10,0.48)

ale=c()
p1<-function(r)
{Fx=pbinom(x,n,p)
Fx1=c(0,Fx)
for(i in 1:length(x))
{
  if((Fx1[i]<=r)&(r<Fx1[i+1]))
  {ale=c(ale,x[i])
  return(ale)}
}
}

resul1=c()
rbino1<-function(rn)
{
  for(i in 1:length(rn))
  {
    resul=p1(rn[i])
    resul1=c(resul1,resul)
  }
  return(resul1)
}

## Un caso binomial con valores de la uniforme continua conocidos

n=10
p=0.48
x=0:10
random1=c(0.081,0.1,0.35,0.82,0.99)
rsimulada=rbino1(random1)
rsimulada

## Un caso aleatorio segun la funcion R
rbinom(5,10,0.48)

###################################
## Simulacion con v.as Continuas ##
###################################

# Simulacion de muestras de tamano n de una exp(1)
# Comparando el metodo de clase con la funcion rexp

n=1000
u=runif(n)
e=-log(1-u)
e1=rexp(n,1)
par(mfrow=c(1,2))
hist(e,main="Método de Clase",xlab="e",ylab="Frecuencia",
     col=c("#FD6467","#D67236"))
hist(e1,main="Con la funcion rexp",xlab="e1",ylab="Frecuencia",
     col=c("#79402E","#B6854D"))
par(mfrow=c(1,1))

## Con la distribucion Weibull(r=0,Theta=1,u=1/alfa)

we<-function(n,alfa){
  U<-runif(n)
  y<-(-log(U))^(1/alfa)
  return(y)
}

RNGkind(sample.kind ="Rounding")
set.seed(51)
x<-we(40,2)
x          #### 0.5036212

mean(x)    #### 0.9717394
gamma(1+1/2)

ks.test(x,"pweibull",2,1)  ### KS: pvalor = 0.08161

help("ks.test")

### Con el ejemplo de la guia

## Esta funcion halla la solucion de uno en uno. No hace el
## calculo con un vector de valores uniformes en [0,1]

rclase=function(U){
  if(0<=U & U<(16/41)){ 
    print(-(16-41*U)^0.5)
  } else { 
    print((41*U-16)^0.5)
  } 
}

U=0.325
rclase(U)

## La siguiente funcion si trabaja con vectores de la Unif[0,1]

rclase1<-function(U)
{
  for(i in 1:length(U))
  {
    if(0<=U[i] & U[i]<(16/41))
    {
      print(-(16-41*U[i])^0.5)
    } else {print((41*U[i]-16)^0.5)}
  }
}

U=c(0.39001,0.87802,0.5214,0.00248)
rclase1(U)
U=runif(40)
rclase1(U)

# Grafico de F(x) por tramos

library(ggplot2)
p <- function(x){
  return(ifelse(x >= -5 & x < -4, 0*x,
                ifelse(x >= -4 & x < 0, 16/41-(1/41)*x^2,
                       ifelse(x >= 0 & x <= 5,16/41+(1/41)*x^2,
                              ifelse(x <= 5 , 0,0)))))}
ggplot(data.frame(x=c(-5, 5)), aes(x=x)) + 
  geom_path(aes(colour='tomato'), stat="function", fun=p,) +
  labs(x="Adelantos y Atrasos", y="Distrib. Acumulada") +
  ggtitle("Distribucion Acumulada") +
  scale_colour_identity("Function", guide="legend", 
                        labels = c("F(x)")) +
  theme(legend.text=element_text(size = 8.5)) +
  theme(plot.title=element_text(size=10, face="bold")) +
  theme(axis.title.x=element_text(face="italic", size=9.5)) +
  theme(axis.title.y=element_text(size=9.5))

# Gráfico de f(x) e histograma

rclase1 <- function(U) {
  x <- numeric(length(U)) # Vector para almacenar las muestras generadas
  for (i in 1:length(U)) {
    if (0 <= U[i] & U[i] < (16 / 41)) {
      x[i] <- -(16 - 41 * U[i])^0.5
    } else {
      x[i] <- (41 * U[i] - 16)^0.5
    }
  }
  return(x)
}

set.seed(44) # Fijar semilla para reproducibilidad
U <- runif(800)

# Generar la muestra utilizando la función rclase1
muestras <- rclase1(U)

data <- data.frame(x = muestras)

# Definir la densidad teórica
densidad_teorica <- function(x) {
  ifelse(x >= -4 & x <= 5, 2 * abs(x) / 41, 0)
}

# Crear datos para la densidad teórica
x_teorico <- seq(-4, 5, length.out = 1000)
y_teorico <- densidad_teorica(x_teorico)
densidad_data <- data.frame(x = x_teorico, y = y_teorico)

# Graficar el histograma y la densidad
ggplot(data, aes(x = x)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "green2", color = "black", alpha = 0.7) +
  geom_line(data = densidad_data, aes(x = x, y = y), color = "tomato", size = 1) +
  labs(title = "Histograma y Densidad Teórica",
       x = "x",
       y = "Densidad") +
  theme_minimal()

#Algoritmo de Box-Muller. Genera n numeros aleatorios de 2 
#distribuciones normales que tienen la misma media mu y 
#desviacion estandar sigma

ABM<-function(n,mu,sigma){
  z1<-c()
  z2<-c()
  for(i in 1:n){
    U1<-runif(1)
    U2<-runif(1)
    R<-sqrt(-2*log(U1))
    Theta<-2*pi*U2
    z1[i]<-R*cos(Theta)
    z2[i]<-R*sin(Theta)
  }
  x<-z1*sigma+mu
  y<-z2*sigma+mu
  uniformes=data.frame(U1,U2)
  X = data.frame(x,y)
  return(list(UnifCorrida_n=uniformes,Valores=X))
}

ABM(3,1.65,0.07)
d4<-ABM(35,1.65,0.07)
shapiro.test(d4$Valores$x)
shapiro.test(d4$Valores$y)
cor(d4$Valores$x,d4$Valores$y)
cor.test(d4$Valores$x,d4$Valores$y)

library(psych)
d5<-cbind(d4$Valores$x,d4$Valores$y)
multi.hist(d5)

### DE OTRA MANERA

# Definir la función para generar una muestra de una distribución normal utilizando el método Box-Muller
box_muller <- function(n=10, mean1=2, sd1=4,mean2=8, sd2=3) {
  u1 <- runif(n)
  u2 <- runif(n)
  #uniformes=data.frame(u1,u2)
  z1 <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  z2 <- sqrt(-2 * log(u1)) * sin(2 * pi * u2)
  X1 <- mean1 + sd1 * z1
  X2 <- mean2 + sd2 * z2
  
  X = data.frame(X1,X2)
  #return(list(UnifCorrida_n=uniformes,Valores=X))
  return(list(x1 = X1, x2 = X2))
}

# Generar una muestra de tamaño 10 de dos distribuciones normales diferentes
muestras <- box_muller()

# Imprimir la muestra generada
print(muestra)

#Datos normales multivariados
library(MASS)
vm<-c(84,8)# Una distribucion normal con media 84 y otra con media 8
sig<-matrix(c(5,2,2,3),2,2) #Matriz varianza covarianza
mvrnorm(10,vm,sig)#Muestra bivariada de tamano 10

# Bootstrap ---------------------------------------------------------------

test_funcion = function(n, k, alfa = 0.05){
  k_uso = NULL
  pvalor1 = NULL; pvalor2 = NULL; resultado = NULL
  for (i in 1:k) {
    k_uso[[i]]=ABM(n,mu=1.65,sigma=0.07)
    pvalor1[[i]] = shapiro.test(k_uso[[i]]$Valores$x)$p.value
    pvalor2[[i]] = shapiro.test(k_uso[[i]]$Valores$y)$p.value
    
    resultado = list(
      normales1 = table(pvalor1 > alfa),
      normales2 = table(pvalor2 > alfa)
    )
    
  }
  
  
  return(resultado)
}

# Al 5%
25/500
test_funcion(n = 30, k = 500)

# al 1% 
8/500
test_funcion(n = 30, k = 500, alfa = 0.01)

## DISTRIBUCION DE LA MEDIA MUESTRAL (SIMULACIONES)

# Seleccion con reemplazo de muestras de tamano 2

poblacion=c(5,8,10)
probab=c(0.4,0.1,0.5)
cremp=sapply(1:1e6,function(x){mean(sample(poblacion,2,T,prob=probab))})
mean(cremp)
var(cremp)

# Seleccion sin reemplazo de muestras de tamano 2

poblacion1=c(rep(5,40),rep(8,10),rep(10,50))
sremp=replicate(1e6,mean(sample(poblacion1,2,F)))
mean(sremp)
var(sremp)

####################################
#                                  #
# LEY DE LOS GRANDES NUMEROS       #
####################################

# Lanzamiento de una moneda con probabilidad p de cara

library(tidyverse)

# Definir el número de lanzamientos y la probabilidad de cara
n_lanzamientos <- 1000
p <- 0.7

# Simular los lanzamientos de la moneda
lanzamientos <- rbinom(n_lanzamientos, size = 1, prob = p)
head(lanzamientos)
# Calcular la proporción acumulada de caras
proporcion_acumulada <- cumsum(lanzamientos) / seq_along(lanzamientos)
head(proporcion_acumulada, 15)
tail(proporcion_acumulada, 15)

datos <- tibble(
  Lanzamiento = seq_along(lanzamientos),
  Proporcion = proporcion_acumulada
)

# Graficar la proporción acumulada de caras
ggplot(datos, aes(x = Lanzamiento, y = Proporcion)) +
  geom_line(color = "green2", lwd = 0.9) +
  geom_hline(yintercept = p, linetype = "dashed", color = "tomato", lwd = 0.9) +
  labs(
    title = "Ley de los Grandes Números: Proporción de Caras en Lanzamientos de una Moneda",
    x = "Número de Lanzamientos",
    y = "Proporción Acumulada de Caras"
  ) +
  theme_minimal() +
  ylim(c(0, 1))

# 2. Simulando valores de una variable discreta, se observa que con muestras
# grandes las proporciones de los valores discretos se parecen a los reales.

library(ggplot2)
#Simulando una variable discreta
x=-4:4
fx=c(0.012,0.025,0.038,0.114,0.416,0.224,0.114,0.052,0.005)
sum(x*fx)
sample(x,size = 10,prob = fx,replace = T)
veces=12000#Probar con otros valores
resultados=sample(x,size = veces,prob = fx,replace = T)
table(resultados)
prop.table(table(resultados))

#Grafico de barras
tabla=prop.table(table(resultados))
proporcion=as.vector(tabla)
df=data.frame(x=as.numeric(names(tabla),y=proporcion))

ggplot(df,aes(x=x,y=proporcion))+
  geom_segment(aes(x=x,xend=x,y=0,yend=proporcion),colour="steelblue",lwd=2)+
  scale_x_continuous(breaks = x,labels=x)

## Simulacion de P(X<=-1)

P=function(n) {
  respuesta=c(rep(0,n))
  for (k in 1:n) {
    muestra=sample(-4:4,k,replace=T,
                   prob = c(0.012,0.025,0.038,0.114,0.416,0.224,0.114,0.052,0.005))
    respuesta[k]=mean(muestra<=-1)
    print(respuesta[k])
  }
  return (respuesta)
}

set.seed(44)
n=1000

A=P(n)
head(A,24)
tail(A,24)
x=-4:4
fx=c(0.012,0.025,0.038,0.114,0.416,0.224,0.114,0.052,0.005)
Fx=cumsum(fx)
cbind(x,fx,Fx)
(parametro=cumsum(fx)[4])

## Grafico
parar_en1=n
df=data.frame(TamMuestra=1:parar_en1,
              probabilidad=A)
head(df)
library(ggplot2)
ggplot(df,aes(x=TamMuestra,y=probabilidad))+
  labs(x='Tamano de la Muestra',y='Proporciones',
       title='La LGN con Proporciones de una Distribucion Discreta')+
  geom_line(colour="green2")+
  geom_hline(yintercept = parametro,colour="tomato",lwd=1.4)+
  lims(y=c(0,0.52))+
  theme_dark()

## SIMULACION DE LA MEDIA DE X [(a+b+c)/3]. Distribucion triangular

library(tidyverse)
library(EnvStats)

# Parámetros de la distribución triangular
a <- 0  # límite inferior
b <- 10 # límite superior
c <- 5  # modo (valor donde la función de densidad es máxima)

# Número máximo de muestras
n_max <- 2000

# Simular datos de la distribución triangular
set.seed(123)  # para reproducibilidad
muestras <- rtri(n_max, min = a, mode = c, max = b)

# Calcular medias muestrales
medias_muestrales <- cumsum(muestras) / 1:n_max

# Media teórica de la distribución triangular
media_teorica <- (a + b + c) / 3

df <- tibble(
  TamañoMuestra = 1:n_max,
  MediaMuestral = medias_muestrales
)

ggplot(df, aes(x = TamañoMuestra, y = MediaMuestral)) +
  geom_line(color = "green2", lwd = 0.9) +
  geom_hline(yintercept = media_teorica, color = "tomato", 
             linetype = "dashed", lwd = 0.9) +
  labs(
    title = "Convergencia de la Media Muestral a la Media Teórica",
    x = "Tamaño de la Muestra",
    y = "Media Muestral"
  ) +
  theme_minimal() +
  annotate("text", x = n_max*0.8, y = media_teorica, 
           label = paste("Media Teórica =", 
                         round(media_teorica, 2)), vjust = -1, color = "blue")

###########################################
## TEOREMA DEL LIMITE CENTRAL            ##
###########################################

# Se muestran distribuciones muestrales del promedio con r muestras
# de tamano n simulados de la dist. cualquiera. en este caso la 
# simulacion se hace con una distrib. Poisson(3)

# poblacion=N, r= # de muestras de tamano n que se van a usar
# n=Tamano de las muestras.
# Con una primera salida

DMM<-function(poblacion,r,n){
  N<-length(poblacion)
  mu<-mean(poblacion)
  sig2<-var(poblacion)*((N-1)/N)
  medias<-c()
  for(i in 1:r){
    medias[i]<-mean(sample(poblacion,n,replace=T))
  }
  Mm<-mean(medias)
  Vm<-var(medias)*((r-1)/r)
  com1<-c(mu,Mm)         ## Comparacion1
  com2<-c(sig2/n,Vm)     ## Comparacion2
  par(mfrow=c(1,2))
  hist(poblacion,col="green")
  hist(medias,col="steelblue")
  shap=shapiro.test(medias)
  X=data.frame(com1,com2)
  Y=data.frame(shap$p.value)
  return(list(Resultados=X,pvalor=Y))
}

RNGkind(sample.kind ="Rounding")
set.seed(2021)
N=4000
Poblacion<-rpois(N,3)#Probar con otras distribuciones: exp, unif, pareto etc.
DMM(Poblacion,1000,5)
DMM(Poblacion,1000,10)
DMM(Poblacion,1000,200)

## TLC de un promedio muestral con una segunda salida

DMM2<-function(poblacion,r,n){
  library(kableExtra)
  N<-length(poblacion)
  mu<-mean(poblacion)
  sig2<-var(poblacion)*((N-1)/N)
  medias<-c()
  for(i in 1:r){
    medias[i]<-mean(sample(poblacion,n,T))
  }
  media_estimada<-mean(medias)
  variancia_estimada<-var(medias)*((r-1)/r)
  promedios<-c(mu,media_estimada)
  varianzas<-c(sig2/n,variancia_estimada)
  desviaciones=c((sig2/n)^0.5,variancia_estimada^0.5)
  par(mfrow=c(1,2))
  hist(poblacion,col="darkolivegreen1")
  hist(medias,col="dodgerblue1")
  shap=shapiro.test(medias)
  #X=data.frame(promedios,varianzas,desviaciones)
  pvalor=c(NA,shap$p.value)
  #Y=data.frame(pvalor)
  tabl <- data.frame("Medias" = promedios,
                     "Desviacion Estandar" = desviaciones,
                     "Varianza" = varianzas,
                     "pvalor"=pvalor,
                     row.names = c("Teoricas", "Muestrales"))
  tabl %>% kbl() %>% 
    kable_styling(latex_options = "striped")
}

RNGkind(sample.kind ="Rounding")
set.seed(2021)
pobla3=rnorm(800,4.8,1.2)
DMM2(pobla3,500,10)
DMM2(pobla3,500,32)

######################################################
#   Distribucion Muestral de la varianza insesgada   #
######################################################

RNGkind(sample.kind ="Rounding")
set.seed(400)

dvm<-function(poblacion,r,n){
  N<-length(poblacion)
  mu<-mean(poblacion)
  mu4=mean((poblacion-mu)^4)
  sig2<-var(poblacion)*((N-1)/N)
  sig4=sig2^2
  varianzas<-c()
  for(i in 1:r){
    varianzas[i]<-var(sample(poblacion,n,T))
  }
  Ev<-mean(varianzas)
  Vv<-var(varianzas)
  com1<-c(sig2,Ev)         ## Comparacion1
  com2<-c((mu4-(n-3)*sig4/(n-1))/n,Vv)     ## Comparacion2
  par(mfrow=c(1,2))
  hist(poblacion,col="goldenrod1")
  hist(varianzas,col="goldenrod3")
  shap=shapiro.test(varianzas)
  Y=data.frame(shap$p.value)
  X=data.frame(com1,com2)
  return(list(Resultados=X,pvalor=Y))
}

poisson=rpois(500,8.2)
estatura<-rnorm(500,1.60,0.07)
dvm(poisson,2000,42)
dvm(poisson,2000,1000)
dvm(estatura,2000,42)
dvm(estatura,2000,1000)

## La Distribucion Ji Cuadrado

# Suma de 10 Normales Estándard al Cuadrado

library(tidyverse)

n <- 10  # grados de libertad
n_samples <- 10000  # número de muestras

# Simular n_samples vectores de n variables normales estándar
set.seed(123)  # para reproducibilidad
normales <- matrix(rnorm(n * n_samples), ncol = n)

# Calcular la suma de los cuadrados de cada fila
sumas_cuadrados <- rowSums(normales^2)

df <- tibble(SumasCuadrados = sumas_cuadrados)

# Crear el histograma con la densidad superpuesta
ggplot(df, aes(x = SumasCuadrados)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dchisq, args = list(df = n), color = "red", size = 1) +
  labs(
    title = paste("Distribución de la Suma de", n, "Variables Normales Estándar al Cuadrado"),
    x = "Suma de Cuadrados",
    y = "Densidad"
  ) +
  theme_minimal() +
  annotate("text", x = max(sumas_cuadrados)*0.8, y = 0.1, label = paste("Distribución Chi-cuadrado con", n, "grados de libertad"), vjust = -1, color = "red")

## LA DISTRIBUCION t

# Definir el rango de valores para el eje x
x <- seq(-4, 4, length = 1000)

# Calcular las densidades
df1 <- 3   # Grados de libertad para la primera distribución t
df2 <- 5   # Grados de libertad para la segunda distribución t
df3 <- 10  # Grados de libertad para la tercera distribución t

densidades <- tibble(
  x = x,
  Normal = dnorm(x),
  t_df1 = dt(x, df = df1),
  t_df2 = dt(x, df = df2),
  t_df3 = dt(x, df = df3)
)

# Convertir el data frame a formato largo para ggplot2
densidades_long <- tidyr::pivot_longer(densidades, cols = -x, 
                                       names_to = "Distribución", values_to = "Densidad")

ggplot(densidades_long, aes(x = x, y = Densidad, color = Distribución)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Normal" = "black", "t_df1" = "red", 
                                "t_df2" = "blue", "t_df3" = "green"),
                     labels = c("Normal Estándar", paste("t con", df1, "gl"),
                                paste("t con", df2, "gl"), paste("t con", df3, "gl"))) +
  labs(
    title = "Comparación de Densidades t con Diferentes Grados de Libertad y una Normal Estándar",
    x = "x",
    y = "Densidad",
    color = "Distribución"
  ) +
  theme_minimal()

## LA DISTRIBUCION t NO CENTRAL

library(tidyverse)
# Parámetros
df_values <- c(5, 10, 15, 20)  # grados de libertad
ncp_values <- c(0, 2, 4, 6)    # parámetros de no centralidad
x <- seq(-10, 10, length.out = 1000)  # rango de valores para calcular las densidades

# Crear un data frame para almacenar las densidades
df <- data.frame(x = rep(x, times = length(df_values)),
                 df = factor(rep(df_values, each = length(x))),
                 ncp = factor(rep(ncp_values, each = length(x) * length(df_values) / length(ncp_values))),
                 density = NA)

# Calcular las densidades t no centrales
for (i in 1:length(df_values)) {
  for (j in 1:length(ncp_values)) {
    subset_idx <- df$df == df_values[i] & df$ncp == ncp_values[j]
    df$density[subset_idx] <- dt(x, df = df_values[i], ncp = ncp_values[j])
  }
}

ggplot(df, aes(x = x, y = density, color = interaction(df, ncp))) +
  geom_line(size = 1) +
  labs(
    title = "Densidades t no centrales",
    x = "Valor",
    y = "Densidad",
    color = "df, ncp"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

### LA DISTRIBUCION F

library(tidyverse)
# Grados de libertad para las distribuciones F
df1 <- list(c(5, 10), c(10, 5), c(20, 10), c(10, 20))
labels <- c("df1 = 5, df2 = 10", "df1 = 10, df2 = 5", "df1 = 20, df2 = 10", "df1 = 10, df2 = 20")

# Generar una secuencia de valores x
x <- seq(0, 5, length.out = 1000)

# Crear un data frame para almacenar las densidades
df <- data.frame(x = rep(x, times = length(df1)),
                 density = unlist(lapply(df1, function(d) df(x, d[1], d[2]))),
                 group = rep(labels, each = length(x)))

ggplot(df, aes(x = x, y = density, color = group)) +
  geom_line(size = 1) +
  labs(
    title = "Densidades F con Diferentes Grados de Libertad",
    x = "x",
    y = "Densidad",
    color = "Grados de Libertad"
  ) +
  theme_minimal()

####################################################################
#  ESTADISTICAS DE ORDEN CON VARIABLES ALEATORIAS DISCRETAS        #
####################################################################

(v1=pbinom(4,20,0.24))
(v2=dbinom(5,20,0.24))
(v3=pbinom(5,20,0.24,lower.tail = F))

A<-function(v1,v2,v3){
  respuesta<-c()
  for(i in 1:15){
    respuesta[i]<-(choose(18,i-1)*v3^(i-1)*(v1+v2)^(18-i+1))
  }
  respuesta1=cumsum(respuesta)
  return(respuesta1[15])
}

A(v1,v2,v3)

## DE OTRA MANERA
(v1=pbinom(4,20,0.24))
(v2=dbinom(5,20,0.24))
(v3=pbinom(5,20,0.24,lower.tail = F))
v4=v1+v2
i=(0:14)
library(combinat)
sum(nCm(18,i)*v3^i*v4^(18-i))

###############################################################
## Hallar la funcion de probabilidad de estadisticas de orden
## discretas
###############################################################

### Estadisticas de orden para una distribucion binomial

# k=orden del estadistico, x=valor de la v.a binomial, N=tamaño de muestra,
# n=número de ensayos de Bernoulli, p=probabilidad de éxito

library(tidyverse)

fmp.yk.binom <- function(k, x, N, n, p) {
  pbeta(pbinom(x, n, p), k, N - k + 1) - pbeta(pbinom(x, n, p) - dbinom(x, n, p), k, N - k + 1)
}

# Parámetros
N <- 18  # tamaño de la muestra
n <- 20  # número de ensayos en la distribución binomial
p <- 0.24 # probabilidad de éxito en cada ensayo
k <- 3   # estadística de orden (por ejemplo, la tercera más pequeña)

x_vals <- 0:n

# Calcular la fmp para cada valor de x
fmp_vals <- sapply(x_vals, function(x) fmp.yk.binom(k, x, N, n, p))

# Calcular la fda
fda_vals <- cumsum(fmp_vals)

df <- data.frame(
  x = x_vals,
  fmp = fmp_vals,
  fda = fda_vals
)

# Graficar la fmp
ggplot(df, aes(x = x, y = fmp)) +
  geom_point(color = "blue", size = 3) +
  geom_segment(aes(x = x, xend = x, y = 0, yend = fmp), color = "blue") +
  labs(
    title = paste("Función Masa de Probabilidad de la", k, "ésima Estadística de Orden"),
    x = "Valores de X",
    y = "FMP"
  ) +
  theme_minimal()

# Graficar la FDA
ggplot(df, aes(x = x, y = fda)) +
  geom_point(size = 1.5,
             color = '#FF1493',
             alpha = 0.6,
             shape = 21,
             fill = '#EE2C2C',
             stroke = 2) +  # Puntos para la FDA 
  geom_segment(aes(xend = lead(x, default = last(x)), yend = fda),
               linetype = "solid",
               color = '#1874CD',
               linewidth = 1.2) +
  geom_segment(aes(x = x, xend = x, y = 0, yend = fda), linetype = "dotted") +
  labs(x = "Valores de X", y = "FDA", title = "Función de Distribución Acumulada (FDA)") +
  theme_minimal()

### Haciendo simulaciones para comparar resultados
### Binomial

# N=tamaño de muestra, n=número de ensayos de Bernoulli,
# p=probabilidad de éxito,k=orden del estadístico
N = 22; n = 50; p = 0.6; k = 14
x=0:n
y=numeric(length(x))
for(i in x) {y[i+1] = fmp.yk.binom(k, i, N, n, p)}
yk=function(x,k) apply(x, 1, function(x) sort(x,partial=k)[k])
nSim=5000
tmp=yk(matrix(rbinom(N*nSim, n, p),ncol=N),k)
head(tmp, 100)
hist(tmp, probability = T, breaks = seq(min(tmp)-0.5,max(tmp)+0.5), 
     col = 'skyblue', main = paste("Función Masa de Probabilidad de la", 
     k, "ésima Estadística de Orden"), ylim = c(0, 0.45))
points(x, y, type = 'b',lwd = 3.5, col='tomato')

