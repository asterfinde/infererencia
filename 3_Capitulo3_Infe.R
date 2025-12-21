
######################################################
## Curso: Inferencia Estadistica                    ##
## Profesor: Clodomiro Fernando Miranda Villagomez  ##
######################################################

##########################################
##         CAPITULO 3                   ##
##   Intervalos de Confianza            ##
##########################################

library(tidyverse)
library(ggplot2)
library(patchwork)

mu=25
sigma=4
N=1000 # numero de muestras de tamano n
n=30 # tamano de muestra
contador=0
df=tibble(x=c(22,28),y=c(1,100))
p=ggplot(df,aes(x=x,y=y))+
  geom_vline(xintercept=mu,color='tomato',lwd=0.08)

for(i in 1:N){
  x=rnorm(n,mu,sigma)
  Linf=mean(x)-1.96*sigma/sqrt(n)
  Lsup=mean(x)+1.96*sigma/sqrt(n)
  if(Linf<mu && mu<Lsup) # chequear que 25 este dentro del intervalo
    contador=contador+1
  if(i<=100) # graficar los primeros 100 intervalos
    p=p+annotate('segment',x=Linf,xend=Lsup,y=i,yend=i)
}

p
# Porcentaje de intervalos que incluyen a mu=25
# Aproximadamente 95%
(contador/1000)*100 

pnorm(1.96)

# A continuacion se aprecia que cuando sigma de una poblacion 
# normal no se conoce entonces cuando los grados de libertad
# de la distribucion t tiende al infinito la distribucion t
# se aproxima a la normal estandar.
N=10^4
mu=25
sigma=7
t=numeric(N)
n=15 # luego probar con n=80
for(i in 1:N){
  x=rnorm(n,mu,sigma)
  Xbar=mean(x)
  s=sd(x)
  t[i]=(Xbar-mu)/(s/sqrt(n))
}
df=tibble(t)
a=ggplot(df,aes(t))+
  geom_histogram(bins = 10,fill='purple',col='green2')
b=ggplot(df,aes(sample=t))+
  geom_qq(col='skyblue')+geom_qq_line(col='tomato')
b
library(ggExtra)
ggMarginal(b,type='histogram')

c=ggplot()+
  xlim(-5,5)+
  geom_function(aes(color='Normal'),fun=dnorm,lty=1,lwd=1.2)+
  geom_function(aes(color='t, df = 4'),fun=dt,args=list(df=4),lty=2,lwd=1.2)+
  geom_function(aes(color='t, df = 8'),fun=dt,args=list(df=8),lty=3,lwd=1.2)+
  geom_function(aes(color='t, df = 16'),fun=dt,args=list(df=16),lty=1,lwd=1.2)

(a + b) / c

# Halle un intervalo confidencial de 99% para el peso medio de
# ninas nacidas en Carolina del Norte en el 2004. Interprete

library(resampledata)
library(ggplot2)
library(tidyverse)
library(ggstatsplot)
library(effectsize)

head(NCBirths2004)
pesos=NCBirths2004 %>% filter(Gender=='Female') %>%
  pull(Weight)
hist(pesos)
t.test(pesos,conf.level=0.99)$conf
(n=length(pesos))

# Prueba t univariada H0: mu=3450
gghistostats(data = pesos %>% as_tibble(),
             x=value,
             test.value = 3450,  # 0 es por default
             type = 'p',      # 'np' para Wilcoxon
             normal.curve = T,
             normal.curve.args = list(linewidth=2,col='purple'))

interpret_cohens_g(-0.11)# el efecto del peso es pequeño para rechazar H0
?interpret_cohens_d

d=pesos %>% as_tibble()
t.test(d$value,mu=3450)

# Prueba Wilcoxon univariada. H0: Mediana=6
gghistostats(data = pesos %>% as_tibble(),
             x=value,
             test.value = 3450,
             type = 'np',    # 'p' para t
             normal.curve = T,
             normal.curve.args = list(linewidth=2,col='purple'))

interpret_rank_biserial(-0.15)# el efecto del peso es pequeño para rechazar H0

?interpret_rank_biserial

wilcox.test(d$value,mu=3450)

# El intervalo de confianza t da una buena estimación
# cuando se trabaja con una muestra aleatoria (iid) 
# Chequeando las condiciones
df=tibble(pesos)
ggplot(df,aes(pesos))+
  geom_histogram(bins = 10,fill='purple',col='green2')
p=ggplot(df,aes(sample=pesos))+
  geom_qq(col='skyblue')+geom_qq_line(col='tomato')
p
library(ggExtra)
ggMarginal(p,type='histogram')

p=ggplot(df,aes(sample=pesos))+
  stat_qq(distribution=qt,dparams=n-1,col='skyblue')+
  stat_qq_line(distribution=qt,dparams=n-1,col='tomato')
p
library(ggExtra)
ggMarginal(p,type='histogram')

# Extrayendo muestras aleatorias de una distribucion
# NO Normal (gamma)

muyBajo=0
muyAlto=0
n=20
q=qt(0.975,n-1)
N=10^5
for(i in 1:N){
  x=rgamma(n,shape=5,rate=2)
  Xbar=mean(x)
  s=sd(x)
  Linf=Xbar-q*s/sqrt(n)
  Lsup=Xbar+q*s/sqrt(n)
  if(Lsup<5/2)
    muyBajo=muyBajo+1
  if(Linf>5/2)
    muyAlto=muyAlto+1
}

# Aproximadamente el 95% de intervalos diferentes incluyen
# a la media poblacional
muyBajo/N
muyAlto/N

# Con Uniforme[0,1]. Dist. Simetrica
N=10^4
n=10
mu=0.5
y=numeric(N)
for(i in 1:N){
  x=runif(n)
  Xbar=mean(x)
  s=sd(x)
  y[i]=(Xbar-mu)/(s/sqrt(n))
}
library(dplyr)
library(ggplot2)
df=tibble(y)
p=ggplot(df,aes(sample=y))+
  stat_qq(distribution=qt,dparams=n-1,col='skyblue')+
  stat_qq_line(distribution=qt,dparams=n-1,col='tomato')
p
library(ggExtra)
ggMarginal(p,type='histogram')

# Exponencial(media=4). Distrib. Asimetrica
N=10^4
n=10
mu=4
y=numeric(N)
for(i in 1:N){
  x=rexp(n,1/mu)
  Xbar=mean(x)
  s=sd(x)
  y[i]=(Xbar-mu)/(s/sqrt(n))
}
df=tibble(y)
p=ggplot(df,aes(sample=y))+
  stat_qq(distribution=qt,dparams=n-1,col='skyblue')+
  stat_qq_line(distribution=qt,dparams=n-1,col='tomato')
p
library(ggExtra)
ggMarginal(p,type='histogram')

# Exponencial(media=4). Con muestra grande
N=10^4
n=100
mu=4
y=numeric(N)
for(i in 1:N){
  x=rexp(n,1/mu)
  Xbar=mean(x)
  s=sd(x)
  y[i]=(Xbar-mu)/(s/sqrt(n))
}
df=tibble(y)
p=ggplot(df,aes(sample=y))+
  stat_qq(distribution=qt,dparams=n-1,col='skyblue')+
  stat_qq_line(distribution=qt,dparams=n-1,col='tomato')
p
library(ggExtra)
ggMarginal(p,type='histogram')

## INTERVALOS DE CONFIANZA PARA UNA DIFERENCIA DE MEDIAS

# Un educador condujo un experimento para estudiar ciertos 
# aspectos de la habilidad lectora de niños. A 21 niños de
# tercer grado se les aplicó nuevas actividades dirigidas
# de lectura durante 8 meses y a 23 niños de tercer grado
# no se les aplicó. La variable Response muestra las notas.

# Analice las siguientes salidas R.
# Son muestras independientes?
library(resampledata)
library(ggplot2)
library(tidyverse)
head(Reading)
summary(Reading)
df=tibble(Reading)

g=ggplot(df, aes(x = Response,col=Treatment)) 
g+ stat_ecdf(lwd=1.5)

g=ggplot(df, aes(x = Response,col=Treatment)) 
g+ stat_ecdf(geom="point")

# Interprete
t.test(Response~Treatment,data=Reading)$conf
control=Reading %>% filter(Treatment=='Control') %>% pull(Response)
tratado=Reading %>% filter(Treatment=='Treated') %>% pull(Response)
t.test(control,tratado)$conf

## Poblaciones no normales

# n1=10, n2=10

library(ggplot2)
N=10^4
n1=10
n2=10
dif=numeric(N)
for(i in 1:N){
  x=rexp(n1,1)
  y=rexp(n2,1)
  Xbar=mean(x)
  Ybar=mean(y)
  sdif=sqrt(var(x)/n1+var(y)/n2)
  dif[i]=(Xbar-Ybar-0)/sdif
}
df=tibble(dif)
p1=ggplot(df,aes(sample=dif))+
  stat_qq(distribution=qt,dparams=n1+n2-2,col='skyblue')+
  stat_qq_line(distribution=qt,dparams=n1+n2-2,col='tomato')
p1
library(ggExtra)
ggMarginal(p1,type='histogram')

# n1=10, n2=15

library(ggplot2)
N=10^4
n1=10
n2=15
dif=numeric(N)
for(i in 1:N){
  x=rexp(n1,1)
  y=rexp(n2,1)
  Xbar=mean(x)
  Ybar=mean(y)
  sdif=sqrt(var(x)/n1+var(y)/n2)
  dif[i]=(Xbar-Ybar-0)/sdif
}
df=tibble(dif)
p2=ggplot(df,aes(sample=dif))+
  stat_qq(distribution=qt,dparams=n1+n2-2,col='skyblue')+
  stat_qq_line(distribution=qt,dparams=n1+n2-2,col='tomato')
p2
library(ggExtra)
ggMarginal(p2,type='histogram')

# n1=10, n2=100

library(ggplot2)
N=10^4
n1=10
n2=100
dif=numeric(N)
for(i in 1:N){
  x=rexp(n1,1)
  y=rexp(n2,1)
  Xbar=mean(x)
  Ybar=mean(y)
  sdif=sqrt(var(x)/n1+var(y)/n2)
  dif[i]=(Xbar-Ybar-0)/sdif
}
df=tibble(dif)
p3=ggplot(df,aes(sample=dif))+
  stat_qq(distribution=qt,dparams=n1+n2-2,col='skyblue')+
  stat_qq_line(distribution=qt,dparams=n1+n2-2,col='tomato')
p3
library(ggExtra)
ggMarginal(p3,type='histogram')

library(gridExtra)
grid.arrange(p1,p2,p3,ncol=3)

# Considere los pesos de niños y niñas nacidos en Texas
# en el 2004
# Son muestras independientes?
library(resampledata)
library(tidyverse)
library(ggplot2)
head(TXBirths2004)
dim(TXBirths2004)
summary(TXBirths2004)
boys=TXBirths2004 %>% filter(Gender=='Male') %>% pull(Weight)
girls=TXBirths2004 %>% filter(Gender=='Female') %>% pull(Weight)

# Interprete la siguiente salida R
t.test(boys,conf.level=0.95)$conf
t.test(girls,conf.level=0.95)$conf

df=tibble(boys)
n=length(boys)
ggplot(df,aes(boys))+
  geom_histogram(bins = 10,fill='purple',col='green2')
p=ggplot(df,aes(sample=boys))+
  stat_qq(distribution=qt,dparams=n-1,col='skyblue')+
  stat_qq_line(distribution=qt,dparams=n-1,col='tomato')
p
library(ggExtra)
ggMarginal(p,type='histogram')

df=tibble(girls)
(n=length(girls))
ggplot(df,aes(girls))+
  geom_histogram(bins = 10,fill='purple',col='green2')
p=ggplot(df,aes(sample=girls))+
  stat_qq(distribution=qt,dparams=n-1,col='skyblue')+
  stat_qq_line(distribution=qt,dparams=n-1,col='tomato')
p
library(ggExtra)
ggMarginal(p,type='histogram')

# Interprete la siguiente salida R
N=10^4
muDif=mean(boys)-mean(girls)
dif=numeric(N)
for(i in 1:N){
  x=sample(boys,30,replace = T)
  y=sample(girls,30,replace = T)
  Xbar=mean(x)
  Ybar=mean(y)
  sdif=sqrt(var(x)/n1+var(y)/n2)
  dif[i]=(Xbar-Ybar-muDif)/sdif
}
df=tibble(dif)
(n1=length(boys))
(n2=length(girls))
ggplot(df,aes(dif))+
  geom_histogram(bins = 10,fill='purple',col='green2')
p=ggplot(df,aes(sample=dif))+
  stat_qq(distribution=qt,dparams=n1+n2-2,col='skyblue')+
  stat_qq_line(distribution=qt,dparams=n1+n2-2,col='tomato')
p
library(ggExtra)
ggMarginal(p,type='histogram')

# Se estudia los puntajes de 12 atletas en semifinales
# y finales. Asuma que las 12 atletas es una muestra 
# representativa de la elite de atletas mujeres
# Son muestras independientes?

# Interprete la siguiente salida R
library(resampledata)
semi=Diving2017$Semifinal
final=Diving2017$Final
(dif=final-semi)
mean(dif)
t.test(final,semi,paired = T)$conf
t.test(final,semi,mu=0,paired = T)$conf
t.test(final,semi,mu=0,paired = T)

## (Intervalo Wilson o Score Wilson)

## Intervalos Confidenciales para proporciones

#En 2018 una empresa encuestadora entrevistó a 2348 personas
# (de las cuales respondieron 2193) acerca si están de 
# acuerdo o no con la pena de muerte por asesinato. De los
# que respondieron, 1385 estaban a favor de la pena de muerte.
# Halle un intervalo de 90% confianza para la proporcion
# poblacional a favor de la pena de muerte.

# bilateral
prop.test(1385,2193,conf.level = 0.9)$conf
# unilateral
prop.test(1385,2193,alt='greater',conf.level = 0.9)$conf

## Intervalo Confidencial para la diferencia de proporciones.
# Se investigó si un medicamento es efectivo para reducir
# algunos sintomas despues de una cirugia particular. De 674
# pacientes que recibieron el medicamento, 172 reportaron los
# sintomas depues de la cirugia. De 676 pacientes que 
# recibieron un cuidado estandar, 223 tuvieron sintomas. Halle
# un intervalo de 95% de confianza para la verdadera
# diferencia de proporciones (medicamento-estandar)

prop.test(c(172,223),c(674,676))$conf

qnorm(0.95)
qnorm(0.9)
# Sustento heuristico (no muy riguroso, empirico) para considerar que la 
# disminucion en la longitud del intervalo por cada incremento en una 
# unidad de n empieza a ser pequeña se da cuando n>30
n=c(1,20,40,60,80,100)
x=1/sqrt(n)
plot(x,type="l",col="skyblue",ylim=c(0,1),lwd=3,
     xlab="n",ylab="1/sqrt(n)",
     main="Funcion 1/sqrt(n)",xaxt="n")
axis(1, 1:length(n),n)

# Comparacion de intervalos para mu con varianza conocida (1) y t.
# Cuando n es pequeño la esperanza de la longitud de los intervalos
# normales es menor. Conforme n crece la diferencia es pequeña

n=3:100
r=1000
LIt=c()
LSt=c()
LIN=c()
LSN=c()
Et=c()
EN=c()
for (i in 1:length(n)){
  for(k in 1:r)
  {
    data=rnorm(n[i],0,1)
    LIt[k]=mean(data)-qt(0.975,n[i]-1)*sd(data)/n[i]^0.5
    LSt[k]=mean(data)+qt(0.975,n[i]-1)*sd(data)/n[i]^0.5
    LIN[k]=mean(data)-qnorm(0.975,0,1)*1/n[i]^0.5
    LSN[k]=mean(data)+qnorm(0.975,0,1)*1/n[i]^0.5
  }
  Et[i]=mean(LSt-LIt)
  EN[i]=mean(LSN-LIN)
}

plot(Et,type="l",col="skyblue",ylim=c(0,5),
     xlab="Tamaño de Muestra",ylab="Estimación de E(longitud)",
     xaxt="n")
lines(EN,type="l",col="tomato",pch=2)
axis(1,1:length(n),n)
legend(60,4,c("Intervalo t","Intervalo Normal"),
       col=c("skyblue","tomato"),lty=c(1,1),pch = c(1,2))

#########################################
# Intervalos: Bernoulli-Poisson-Pascal  #
#########################################
#Ejemplo 14
#Primera Forma

y<-4
n<-20
alfa<-0.05
li<-(y/(n-y+1)*qf(alfa/2,2*y,2*(n-y+1)))/
  (1+y/(n-y+1)*qf(alfa/2,2*y,2*(n-y+1)))
ls<-((y+1)/(n-y)*qf(1-alfa/2,2*(y+1),2*(n-y)))/ 
  (1+(y+1)/(n-y)*qf(1-alfa/2,2*(y+1),2*(n-y)))
print(c(li,ls),digits=8)

#Segunda Forma
#install.packages("Hmisc")
library(Hmisc)
x<-4
n<-20
binconf(x,n,method = "exact")
#binconf(x,n,method ="all")
##############################
##############################
# Ejemplo 17
#Primera Forma
p1<- 0.050066054
p2<- 0.050521256
t0<-6
n<-10

li<- 1/(2*n)*qchisq(p2,2*t0)
ls<- 1/(2*n)*qchisq(1-p1,2*(t0+1))
print(c(li,ls),digits = 3)

#Segunda Forma
#install.packages("survival")
library(survival)
k<-6
t<-10
p<-0.89941242
cipoisson(k,t,p,method = "exact")
#cipoisson(k,t,p,method = "all")
###############################
###############################
#Ejemplo 18
t0<-35
t0<-t0-15
k<-15

pi1<-0.3
print(pnbinom(t0,k,pi1),digits=9)

pi2<-0.6
print(1-pnbinom(t0-1,k,pi2),digits=9)
