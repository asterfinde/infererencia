######################################################
## Curso: Inferencia Estadistica                    ##
## Profesor: Clodomiro Fernando Miranda Villagomez  ##
######################################################

#########################################################################

## PROPIEDADES DE LOS ESTIMADORES

# EFICIENCIA
library(ggplot2)
library(dplyr)
library(gridExtra)
N=10^4
n=25
Xbar2=numeric(N)
Max=numeric(N)
for(i in 1:N){
  x=runif(n,0,12)
  Xbar2[i]=2*mean(x)
  Max[i]=((n+1)/n)*max(x)
}
mean(Xbar2)
var(Xbar2)
mean(Max)
var(Max)
df=tibble(Xbar2,Max)
library(gridExtra)
p1=ggplot(df,aes(Xbar2))+
  geom_histogram(binwidth=0.5,center=0.25,color='white')+
  labs(x='Medias')+xlim(8,16)

p2=ggplot(df,aes(Max))+
  geom_histogram(binwidth=0.5,center=0.25,color='white')+
  labs(x='Maximos')+xlim(8,16)
grid.arrange(p1,p2,nrow=2)

# Comparacion de la varianza muestral insesgada(vmi) con 
# la sesgada(vms). Simulando muestras de diferentes tamanos de una
# dist. N(5,81). Las estimaciones de la vms son inferiores que las de
# la vmi. Cuando n crece la diferencia entre los dos es mas pequena
# y los valores de los 2 estimadores se acercan al parametro. La vms
# subestima al parametro pero se aprecia que para valores de n: 300, 
# 1000 y 5000 hay una aparente sobrestimacion, esto no es 
# una contradiccion ya que la subestimacion quiere decir que en 
# promedio se obtiene un valor menor que el parametro.

par(mfrow=c(1,1))
set.seed(1)
n=c(2,10,30,50,100,300,1000,5000)
var1=NULL
var2=NULL
for(k in 1:length(n)){
  data=rnorm(n[k],5,9)
  var1[k]=var(data)
  var2[k]=var(data)*(n[k]-1)/n[k]
}

plot(var1,type="b",col="green2",ylim=c(min(var2),130),
     xlab="Tamano de Muestra",ylab="Estimacion de la Varianza",
     xaxt="n")
lines(var2,type="b",col="tomato",pch=2)
abline(h=81)
axis(1,1:length(n),n)
legend(3,120,c("Var.Insesgada","Var.Sesgada"),
       col=c("green2","tomato"),lty=c(1,1),pch = c(1,2))

## COMPARACION DE ECM's de la vmi y vms

set.seed(21)
N=c(2,10,100,1000,10000,100000,1000000)
n=10
msevar1=NULL
msevar2=NULL
for(k in 1:length(N)){
  var1=c()
  var2=c()
  for(l in 1:N[k]){
    data=rnorm(n,5,9)
    var1[l]=var(data)
    var2[l]=(n-1)*var(data)/n
  }
  msevar1[k]=var(var1)
  msevar2[k]=var(var2)+(mean(var2)-81)^2
}

msevar1
msevar2
plot(msevar1,type="b",col="green2",ylim=c(200,2200),
     xlab="Numero de Simulaciones",ylab="Eficiencia de las Estimaciones",
     main="Eficiencia: Varianza Sesgada e Insesgada",xaxt="n")
lines(msevar2,type="b",col="tomato",pch=2)
axis(1, 1:length(N),N)
legend(2.5,2080,c("Insesgado","Sesgado"),
       col=c("green2","tomato"),lty=c(1,1),pch = c(1,2))


## METODOS DE ESTIMACION

# Se observan aleatoriamente 20 llamadas y se registra el tiempo
# que transcurre antes de ser atendido por un operador.
# Suponga que se tiene que elegir entre la distribucion exponencial
# con media theta y la dist. normal. Con el metodo de maxima
# verosimilitud estime el porcentaje de llamadas que necesitan mas
# de 2 minutos para ser atendidas.

a=c(0.13,0.06,0.5,0.41,1.44,0.6,0.22,1.08,0.78,0.92,2.73,0.83,
    0.19,0.21,1.75,0.79,0.02,0.05,2.3,1.03)
str(a)
mean(a)
hist(a,main = "Histograma",col="yellow")

summary(a)

par(mfrow=c(1,2))
Z <- a      # random sample from exponential distribution
p <- ppoints(100)    # 100 equally spaced points on (0,1),
# excluding endpoints
q <- quantile(Z,p=p) # percentiles of the sample distribution
plot(qexp(p) ,q, main="Exponential Q-Q Plot",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=qexp,col="tomato", lty=2,lwd=2)

plot(qnorm(p) ,q, main="Normal Q-Q Plot",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=qnorm,col="violet", lty=2,lwd=3)

par(mfrow=c(1,1))
Z <- a
library(MASS)
params <- as.list(fitdistr(Z, "exponential")$estimate)

library(ggplot2)
qplot(sample = Z, geom = 'blank') +
  stat_qq(distribution = qexp, dparams = params)

# Se ve que hay un mejor ajuste a la dist. exponencial
# El EMV=media=0.802 entonces la respuesta es:
pexp(2,1/0.802,lower.tail = F)

# Otro ejemplo: Se tiene una muestra de la longitud de 12 laminas
# de vidrio. Suponga que se quiere modelar a la longitud con la dist.
# normal.  Mediante un analisis grafico se aprecia que como la 
# muestra es pequena el histograma no ayuda mucho pero segun el QQnorm
# la dist. normal parece apropiada

vidrio=c(3.56,3.36,2.99,2.71,3.31,3.68,2.78,2.95,2.82,3.45,3.42,3.15)
hist(vidrio,col="steelblue")
qqnorm(vidrio,main="QQ plot para distribucion normal",
       xlab="cuantiles teoricos",ylab="Cuantiles muestrales")
qqline(vidrio, distribution=qnorm,col="green2", lty=2,lwd=2)

# Estime P(X<2.8)
n=12
pnorm(2.8,mean(vidrio),(var(vidrio)*(n-1)/n)^0.5)

# Compare en forma grafica los estimadores de momentos de los
# parametros de las dist Poisson(5) y Poisson(15).
# Se simulan diferentes tamanos de muestra y en cada muestra se 
# calculan los dos estimadores y se observa cual es mas cercano
# al parametro. La linea horizontal representa al parametro.
# Graficamente la media muestral estima mejor al parametro

n=5:300
est.mean=c()
est.var=c()
for(i in 1:length(n)){
  x=rpois(n[i],5)
  est.mean[i]=mean(x)
  est.var[i]=var(x)*(n[i]-1)/n[i]
}

est1.mean=c()
est1.var=c()
for(i in 1:length(n)){
  y=rpois(n[i],15)
  est1.mean[i]=mean(y)
  est1.var[i]=var(y)*(n[i]-1)/n[i]
}

par(mfrow=c(1,2))
plot(n,est.var,xlab="n",ylab="Estimacion",
     main="Poblacion Poisson(5)",
     type="l",col="green2",ylim=c(0,12))
abline(h=5)
lines(n,est.mean,col="tomato")
legend(100,11,c("Media","Varianza"),lty=c(1,1),
       col=c("tomato","green2"),box.col = 0)

plot(n,est1.var,xlab="n",ylab="Estimacion",
     main="Poblacion Poisson(15)",ylim=c(0,45),
     type="l",col="green2")
abline(h=15)
lines(n,est1.mean,col="tomato")
legend(130,38,c("Media","Varianza"),lty=c(1,1),
       col=c("tomato","green2"),box.col = 0)

# Comparacion del estimador de momentos y de maxima verosimilitud
# de una distribucion Uniforme(0,3) y una Uniforme(0,5)

set.seed(456)
n=5:300
est.mome=rep(NA,length(n))
est.maxv=rep(NA,length(n))
for(i in 1:length(n)){
  x=runif(n[i],0,3)
  est.mome[i]=2*mean(x)
  est.maxv[i]=max(x)
}

est1.mome=rep(NA,length(n))
est1.maxv=rep(NA,length(n))
for(i in 1:length(n)){
  y=runif(n[i],0,5)
  est1.mome[i]=2*mean(y)
  est1.maxv[i]=max(y)
}

par(mfrow=c(1,2))
plot(n,est.maxv,xlab="n",ylab="Estimacion",
     main="Poblacion Uniforme(0,3)",
     type="l",col="tomato",ylim=c(2,4.5))
abline(3,0)
lines(n,est.mome,col="green2")
legend(110,4.2,c("Max. Vero","Momentos"),lty=c(1,1),
       col=c("tomato","green2"),box.col = 0)

plot(n,est1.maxv,xlab="n",ylab="Estimacion",
     main="Poblacion Uniforme(0,5)",
     type="l",col="tomato",ylim=c(2,7))
abline(5,0)
lines(n,est1.mome,col="green2")
legend(105,6.5,c("Max. Vero","Momentos"),lty=c(1,1),
       col=c("tomato","green2"),box.col = 0)

################################################################
## Usando R para el estimador maximo verosimil de una Dist. Burr
################################################################

rm(list=ls())

## Generando datos para una distribucion Burr
rBurr=function(n,c,k) (((1-runif(n))^(-1/k))-1)^(1/c)

n=1000;k=1;c=3
xBurr=rBurr(n,c,k)
mean(xBurr)
(mu1=k*beta(k-1/c,1+1/c))
(mu2=k*beta(k-2/c,1+2/c))
var(xBurr)
mu2-mu1^2

# derivada parcial del log(L) con respecto a c (reemplazando k)
c.parm=function(c) n/c+sum(log(xBurr))-
  (n/sum(log(1+xBurr^c))+1)*sum((log(xBurr)*xBurr^c)/(1+xBurr^c))

### PRIMERA MANERA: Con la funcion uniroot
# resolviendo para c
c.mle=uniroot(c.parm,c(0,100))$root

# poniendo el estimador maximo verosimil de c en la derivada parcial de log(L)
# con respecto a k
k.mle=n/sum(log(1+xBurr^c.mle))

# Estimadores Maximo Verosimiles para la Distribucion Burr
c(k.mle,c.mle)

### SEGUNDA MANERA: Con la funcion mle de la libreria stats4
## Otra manera de generar datos de la distribucion Burr
set.seed(1)
library(extremefit)
x=rburr(n,a=3,k=1)
head(x)

##   -log(L)
lBurr=function(a,k)
{
  n=length(x)
  c=a
  k1=k
  (-n*log(c)-n*log(k1)-(c-1)*sum(log(x))+(k1+1)*sum(log(1+x^c)))
}

library(stats4)
estBurr=mle(minuslogl=lBurr,start = list(a=2.8,k=0.9))
summary(estBurr)

#algunos graficos
plot(function(x) dburr(x,3,1), 0, 5,ylab="densidad",
     main = " Densidad Burr ")

plot(function(x) pburr(x,3,1), 0, 5,ylab="funcion de distribucion",
     main = " Distribucion Acumulativa Burr ")

plot(function(x) qburr(x,3,1), 0, 1,ylab="quantil",
     main = " Quantil Burr ")

## EJEMPLO de distribución asintótica
## Optimalidad
# Distribución asintótica del EMV del parámetro de escala theta de una
# distribución Weibull(mu, theta, r), con parámetro de forma mu conocido
# y trasladada a r.

# Distribución asintótica del EMV de theta

# Haciendo simulaciones
n = 150
theta = 4 # scale
mu = 2    # shape
r = 3
theta^2/(n*mu^2)
nSim = 10^5
theta_hat1 = numeric(nSim)
library(FAdist)
for(i in 1:nSim) {
  x = rweibull3(n,shape=mu,scale=theta,thres=r)
  theta_hat1[i] = (sum((x-r)^mu)/n)^(1/mu)
}
(muSimEstim=mean(theta_hat1)) # Media simulada
(varSimEstim=var(theta_hat1)) # Varianza simulada

# Histograma y distribucion aproximada
library(tidyverse)
df <- tibble(theta_hat1)

#----------------
# Graficos - hist & density
ggplot(df, aes(x = theta_hat1))  +
  geom_histogram(aes(y = after_stat(density)),fill='steelblue' ,bins = 10, color = "white") +
  stat_function(fun = dnorm, args = list(mean = theta, sd = sqrt(theta^2/(n*mu^2))),col='green2',lwd=0.8) +
  scale_x_continuous(breaks = c(theta - 3*sqrt(theta^2/(n*mu^2)), theta + 3*sqrt(theta^2/(n*mu^2))))+
  geom_vline(xintercept = theta, color = 'tomato', lty = 2, lwd = 0.8)+
  labs(x = "theta_hat", y = "Densidad")

# Intervalo de confianza con una muestra particular
n = 150
muestra = rweibull3(n,shape=mu,scale=theta,thres=r)
(theta_hat = (sum((muestra-r)^mu)/n)^(1/mu))

alpha = 0.05
(LI = theta_hat - qnorm(1-alpha/2)*sqrt(theta_hat^2/(n*mu^2)))
(LS = theta_hat + qnorm(1-alpha/2)*sqrt(theta_hat^2/(n*mu^2)))

# Prueba de hipótesis
# H0: theta = theta0 = 5
# H0: theta != theta0 = 5
theta0 = 5
(Zc = (theta_hat - theta0) / sqrt(theta0^2/(n*mu^2)))
(pvalor = 2*pnorm(abs(Zc), lower.tail = F))

## Optimalidad

# Ejemplo-a

# Distribución asintótica de los EMV

# Haciendo simulaciones
n = 120
lambda = 8
lambda^2/n
nSim = 10^5
lambda_hat1 = numeric(nSim)
for(i in 1:nSim) {
  x = rbeta(n, shape1 = 8, shape2 = 1)
  lambda_hat1[i] = -n/sum(log(x))
}
(muSimEstim=mean(lambda_hat1)) # Media simulada
(varSimEstim=var(lambda_hat1)) # Varianza simulada

# Histograma y distribucion aproximada
library(tidyverse)
df <- tibble(lambda_hat1)

#----------------
# Graficos - hist & density
ggplot(df, aes(x = lambda_hat1))  +
  geom_histogram(aes(y = after_stat(density)),fill='steelblue' ,bins = 10, color = "white") +
  stat_function(fun = dnorm, args = list(mean = lambda, sd = sqrt(lambda^2/n)),col='green2',lwd=0.8) +
  scale_x_continuous(breaks = c(lambda - 3*sqrt(lambda^2/n), lambda + 3*sqrt(lambda^2/n)))+
  geom_vline(xintercept = lambda, color = 'tomato', lty = 2, lwd = 0.8)+
  labs(x = "lambda_hat", y = "Densidad")

# Con una muestra particular
# Ejemplo-b
n = 120
muestra = rbeta(n, shape1 = 8, shape2 = 1)
(lambda_hat = -n/sum(log(muestra)))

alpha = 0.05
(LI = lambda_hat - qnorm(1-alpha/2)*sqrt(lambda_hat^2/n))
(LS = lambda_hat + qnorm(1-alpha/2)*sqrt(lambda_hat^2/n))

# Ejemplo-c
# H0: lambda = lambda0 = 10
lambda0 = 10
(Zc = (lambda_hat - lambda0) / sqrt(lambda0^2/n))
(pvalor = 2*pnorm(abs(Zc), lower.tail = F))

