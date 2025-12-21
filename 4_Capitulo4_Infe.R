######################################################
## Curso: Inferencia Estadistica                    ##
## Profesor: Clodomiro Fernando Miranda Villagomez  ##
######################################################

#########################
## PRUEBAS DE HIPOTESIS #
#########################

## Prueba de hipótesis para una media con sigma desconocida.

library(resampledata3)
library(ggplot2)
library(tidyverse)
library(ggstatsplot)
library(effectsize)
?Bangladesh
dim(Bangladesh)
head(Bangladesh)
d = Bangladesh$Arsenic
is.data.frame(d)
d1 = tibble(d)
head(d1)
summary(d1)
hist(Bangladesh$Arsenic)

# H0:mu = 100  H1:mu != 100
# Como la distribución es sesgada (asimétrica) no es
# conveniente usar la siguiente prueba t. Se sugiere usar Wilcoxon (ver el
# script del capítulo 3)
t.test(d1, mu = 100, alt = "two.sided") # No usar t

gghistostats(data = d1,
             x=d,
             test.value = 100,
             type = 'np',    # 'p' para t
             normal.curve = T,
             normal.curve.args = list(linewidth=2,col='purple'))

interpret_rank_biserial(-0.37)# el efecto del arsénico grande para rechazar H0

?interpret_rank_biserial

wilcox.test(d1$d,mu=100)
?wilcox.test

# Otra alternativa es: la prueba t Bootstrap aproximada. 
# Esta prueba es recomendable pero no es parte del curso

## En cierta ciudad, el número de nacimientos por mes se modela.
# con una dist. Poisson(lambda). Una persona afirma que 
# lambda es 15 pero se sospecha que lambda es mayor. Si en un
# mes hay 20 nacimientos ¿cuál es la conclusión?

# X~Poisson(lambda)
# Segun una persona H0: lambda=15
# Se sospecha que H1: lambda > 15
# pvalor
(pvalor=ppois(19,15,lower.tail = F))# La persona tiene razón

## Se estudia el contenido de mercurio (ppm) en pescados de dos diferentes 
## lagos. Se tomaron muestras de cada lago.

muestra1=c(0.260,0.263,0.267,0.281,0.288,0.297,0.315,0.315,0.380)
muestra2=c(0.226,0.232,0.246,0.246,0.249,0.256,0.275,0.283,0.302)
(n1=length(muestra1))
(n2=length(muestra2))
(mu1=mean(muestra1))
(mu2=mean(muestra2))
(sd1=sd(muestra1))
(sd2=sd(muestra2))
var.test(muestra1,muestra2)# No tomar en cuenta este resultado
# H0: mu1=mu2     H1: mu1 != mu2
hist(muestra1)
hist(muestra2)
# Como las muestras son del mismo tamaño el hecho las
# distribuciones sean asimétricas se cancela por tener
# muestras de tamaño igual (o también pueden ser de
# tamaño similar). Se recomienda la prueba t
# con varianzas diferentes ya que varianzas iguales es un ideal
t.test(muestra1,muestra2)
t.test(muestra1,muestra2,var.equal = F)

library(tidyverse)
library(performance)
library(dlookr)

df = data.frame(muestra1, muestra2)
d = df %>%
  gather(key='muestra',value = 'mercurio')

d %>%
  group_by(muestra) %>%
  normality(mercurio)

library(ggstatsplot)
library(effectsize)
ggbetweenstats(
  data = d,
  x=muestra,
  y=mercurio,
  type = 'p' # 'np' para Mann Whitney
)

interpret_p(0.02)
?interpret_p

interpret_hedges_g(1.16) # el efecto del lago es grande sobre el mercurio
?interpret_hedges_g

interpret_bf(exp(-1.18)) # factor de sesgo (o Factor Bayes). Similar al pvalor
interpret_bf(exp(1.18))
?interpret_bf

## Se hace la simulación para explorar el efecto del tamaño de muestra
# y el supuesto de varianzas homogéneas (pooled) o heterogéneas
# (unpooled). ¿Qué conclusiones encuentra?

# a
m=30
n=30 # luego cambiar n a 300
mu1=30
mu2=30
sigma1=5
sigma2=5
N=10^5
contador.Pooled=0
contador.Nopooled=0
for (i in 1:N){
  x=rnorm(m,mu1,sigma1)
  y=rnorm(n,mu2,sigma2)
  p.Pooled = t.test(x, y, var.equal = TRUE)$p.value
  p.Nopooled = t.test(x, y, var.equal = FALSE)$p.value
  # Incrementar en 1 si la H0 se rechaza
  contador.Pooled=contador.Pooled+(p.Pooled<0.05)
  contador.Nopooled=contador.Nopooled+(p.Nopooled<0.05)
}
contador.Pooled/N
contador.Nopooled/N
# Con 2 muestras de una misma distribucion normal la 
# proporción de veces que se rechaza H0 es similar. Esto
# se cumple con tamaños de muestra igual o muy diferentes.

# b
m=30
n=30 # después cambiar a 300
mu1=30
mu2=30
sigma1=5
sigma2=12
N=10^5
contador.Pooled=0
contador.Nopooled=0
for (i in 1:N){
  x=rnorm(m,mu1,sigma1)
  y=rnorm(n,mu2,sigma2)
  p.Pooled=t.test(x,y,var.equal = TRUE)$p.value
  p.Nopooled=t.test(x,y,var.equal = FALSE)$p.value
  # Incrementar en 1 si la H0 se rechaza
  contador.Pooled=contador.Pooled+(p.Pooled<0.05)
  contador.Nopooled=contador.Nopooled+(p.Nopooled<0.05)
}
contador.Pooled/N
contador.Nopooled/N
# Con 2 muestras de diferentes distribuciones normales la 
# proporción de veces que se rechaza H0 es similar cuando
# los tamanos de muestra son iguales.

# Con 2 muestras de diferentes distribuciones normales la 
# proporción de veces que se rechaza H0 es diferente cuando
# los tamanos de muestra son muy diferentes.
# Si la distribución normal de menor varianza tiene menor 
# tamaño de muestra la proporción de veces que se rechaza
# la H0 es menor que 0.05 cuando se asume homogeneidad de 
# varianzas y esa proporción es aproximadamente 0.05 cuando
# se asume varianzas heterogeneas.
# Si la distribución normal de mayor varianza tiene menor 
# tamano de muestra la proporción de veces que se rechaza
# la H0 mayor que 0.05 cuando se asume homogeneidad de 
# varianzas y esa proporción es aproximadamente 0.05 cuando
# se asume varianzas heterogéneas.
# Una conclusion es que si los tamaños de muestra son muy
# diferentes hay que asumir varianzas heterogéneas.

## Prueba de hipotesis para una proporción.
# H0: p=0.26 (p=p0)      H1: p<0.26 (p<p0)

# Prueba exacta, con la distribucion Binomial, para proporciones

n=310
(pHat=69/310)
p0=0.26
# X=N° de adultos terminaron secundaria en una muestra
# de tamano 310
# X~Binomial(310,p0)
x=0:n
(k=round(n*pHat))
binom.test(69,310,p0,alternative = 'less')
(pvalor=sum(dbinom(0:k,n,p0)))# Verificando el pvalor
# La proporción de adultos con secundaria no ha cambiado.

# Prueba Z para proporciones
prop.test(69,310,p=0.26,alternative = 'less',correct=T)

## Prueba de hipotesis para una diferencia de proporciones.

prop.test(c(0.62*552,0.68*577),c(552,577),
          alternative = 'two.sided')

## Prueba de hipótesis para una diferencia de proporciones
# con datos pareados.

# Una estudiante posdoctoral conduce una encuesta sobre el
# estrés en su universidad y halla que de 250 estudiantes
# de primer año, 55 de ellos dijeron que toman al menos 
# dos tazas de café al dia. Ella hace una encuesta de
# seguimiento del año siguiente y encuentra que de estos
# 250 estudiantes de segundo año, 71 dijeron que toman al
# menos 2 tazas de café al dia. Ella quiere usar la prueba
# de dos proporciones muestrales para ver si la diferencia
# es estadisticamente discernible, pero ella decide
# consultarle a ud primero. ¿Que le aconsejarias?

# No se debe hacer la prueba de dos proporciones muestrales
# porque se tienen datos pareados que son dependientes. 
# Se debe hacer la prueba t pareada.

# Suponga que 50 estudiantes tomaron café en ambos años: 
# 5 dejaron el café y 21 empezaron con el café. Esto nos
# deja con 26 estudiantes (que dejaron o empezaron con el
# café). 21 incrementaron su consumo y 5 dejaron. Hay 224 
# (174+50=224 que que no cambiaron su costumbre (de tomar 
# o no tomar café)). Los que no cambiaron su comportamiento
# no son importantes en nuestro análisis. Sea X=número de
# estudiantes que incrementaron su consumo en el grupo de 
# 26. Corresponde hacer una prueba de una muestra de una 
# proporción.
# Se tiene H0:p=0.5 y H1:p!=0.5, donde p es la probabilidad 
# de que un estudiante incremente el consumo de café.

# Prueba exacta
# X~Binomial(26,0.5)
n=26;p=0.5
binom.test(21,n,0.5,alternative = 'two.sided')
(pvalor=2*sum(dbinom(21:26,n,p)))# Verificando el pvalor
# También X1=número de estudiantes que dejaron de consumir
# café. X1~Binomial(5,0.5)
binom.test(5,n,0.5,alternative = 'two.sided')
(pvalor1=2*sum(dbinom(0:5,n,p)))
# La probabilidad de que un estudiante incremente su consumo 
# (o deje el café) no es 0.5. La cosa no està 50 a 50.
# Hay diferencias entre los que incrementaron y quienes
# dejaron el consumo de café.

###############################################
#  Ho: mu=120  H1: mu<120 
###############################################

# a. Xbar~Normal(mu0,sig/sqrt(n))
n=16;mu0=120;sig=8
(alpha=pnorm(115,mu0,sig/sqrt(n)))

# b. P(Z>=(3/8)*sqrt(n))
mu0=120;sig=8;mu1=112;beta=0.0197
(q=qnorm(beta,lower.tail=F))
f=function(x) (3/8)*sqrt(x)-q

uniroot(f,interval = c(25,35))$root

# c.
a=0.05;n=30;mu0=120;sig=8;mu1=115
x=seq(100,140,0.1)
z0=dnorm(x,mu0,sig/sqrt(n))
## grafico de la distribucion con H0 verdadera
plot(x,z0,type='l',lwd=3,xlab='valores z',
     ylab='densidad',main='Prueba z con una Muestra')
## sombrear la region de rechazo
polygon(c(qnorm(a,mu0,sig/sqrt(n)),
          seq(0,qnorm(a,mu0,sig/sqrt(n)),1)),
        c(0,dnorm(seq(0,qnorm(a,mu0,sig/sqrt(n)),1),
                  mu0,sig/sqrt(n))),density=80,col='red',angle=135)
legend('topleft',paste('Alpha=',a))

## crear datos para graficar
z1=dnorm(x,mu1,sig/sqrt(n))
## Notar que estamos graficando una distribución 
# Normal(115,sig/sqrt(n))
lines(x,z1,lwd=3,col='blue')

## sombrear en la region de rechazo bajo la distribución
# normal(115,sig/sqrt(n))
polygon(c(qnorm(a,mu0,sig/sqrt(n)),
          seq(0,qnorm(a,mu0,sig/sqrt(n)),1)),
        c(0,dnorm(seq(0,qnorm(a,mu0,sig/sqrt(n)),1),
                  mu1,sig/sqrt(n))),density=80,col='green',angle=135)

(ValorCritico=qnorm(a,mu0,sig/sqrt(n)))
(poder=pnorm(ValorCritico,mu1,
             sig/sqrt(n)))

## volver a sombrear de rojo el area de rechazo para hacerlo mas destacado
polygon(c(qnorm(a,mu0,sig/sqrt(n)),
          seq(0,qnorm(a,mu0,sig/sqrt(n)),1)),
        c(0,dnorm(seq(0,qnorm(a,mu0,sig/sqrt(n)),1),
                  mu0,sig/sqrt(n))),density=80,col='red',angle=135)
legend('topright',c('Bajo H0','Bajo H1',
                    'Region de Rechazo','Poder'),lty=1,
       col=c('black','blue','red','green'),lwd=3)

####################################
# Potencia con una binomial
####################################

# H0:theta=0.9  vs  H1:theta<0.9. Y~Binomial(n=25,theta)

# a. Error tipo I
theta=0.9;n=25
(alpha=sum(dbinom(0:19,n,theta)))

# a. Error tipo II. Y1~Binomial(n=25,p1)
theta1=0.6;n=25
(beta=1-sum(dbinom(0:19,n,theta1)))

##########################################
# Otro ejemplo binomial. Prueba bilateral
##########################################

# H0:p=0.6  vs  H1:p!=0.6. Y~Binomial(n=50,p)
# 0.05=P(Rechazar H0/H0 Verdadera)
# 0.05=P( Y no está cerca de 30=0.6*50/Y~Binomial(50,0.6))
# 0.025=P(Y<=C1/Y~Binomial(50,0.6))

(prob1=round(dbinom(0:24,50,0.6),5))

# 0.025=P(Y>=C2/Y~Binomial(50,0.6))

(prob2=round(dbinom(36:50,50,0.6),5))

# Region Critica={0,1,2,3,...,23}U{37,38,...,50}
# Con v.a.ds no se puede hallar una probabilidad de cometer 
# error tipo I exacta.

dbinom(23,50,0.6)+dbinom(37,50,0.6)# cercana a 0.05

##################################################
## Ejemplo Poisson. Determinacion del valor de k.
##################################################

n=10
lambda=0.12
(x=round(11):0)
prob=dpois(x,n*lambda)
suma_acumulativa=cumsum(prob)

# Valor acumulado es 'menor' o 'mayor' a 0.01
comparacion=ifelse(suma_acumulativa < 0.01, 'Rechazar','No rechazar')

# Tabla
tabla=data.frame(x,prob,suma_acumulativa,comparacion)
names(tabla)=c('x','Probabilidad','Suma Acumulativa','Decision H0')
print(tabla)

# cálculo de gamma
(muestra=4)# 4=4(Y=sum(muestra))
a=ppois(4,n*lambda,lower.tail = F)
b=dpois(4,n*lambda)
f=function(x) {a+b*x-0.01}
library(MASS)
(gamma=uniroot(f,c(0,1))$root)# gama=0.08662322
# No coinciden los valores por los redondeos de la guía
(gama=1/13)

##################################################
# Funcion Potencia. P de H respecto a mu 
##################################################

# Cuando mu se aleja de mu0=120 la potencia es mayor entonces la
# regla de decision es capaz de reconocer H0 falsas. Cuando n es
# más grande entonces la potencia de la prueba se acerca
# a 1, una H0 falsa es facil de detectar.

# a. Poder vs mu
# H1:mu<mu0

po_norm=function(mu,n,mu0,alpha,sigma){
  #1-pnorm((mu0-mu)*sqrt(n)/sigma+qnorm(1-alpha))#+
  pnorm((mu0-mu)*sqrt(n)/sigma-qnorm(1-alpha)) 
}
alpha=0.05
sigma=8
mu0=120
n=30

plot(function(x) po_norm(x,n,mu0,alpha,sigma),112,125,type="l",
     xlab="mu",ylab="funcion potencia",col="tomato",lwd=3)

mu=seq(112,125,0.05)
poder=pnorm((mu0-mu)*sqrt(n)/sigma-qnorm(1-alpha))
library(ggplot2)
library(dplyr)
df=tibble(mu,poder)
head(df,20)
plot(mu,poder,xlab='Probabilidades',
     ylab='Poder de la Prueba',main='Poder vs Probabilidades',
     col='green',type='l',lwd=1.5)

ggplot(data=df,mapping=aes(x=mu,y=poder))+
  geom_line(colour='green',linewidth=1.5)+
  labs(x='Promedios',y='Poder de la Prueba',
       title='Poder vs Medias')

###############################################
#  Funciòn potencia con una binomial
###############################################

# Expresar el poder de la prueba como una función de p.
# poder=1-beta=P(Rechazar H0/H1 es verdadera)
# poder=1-beta=P(Y=0,1,...,19 / theta<0.9)

# Graficar el poder de la prueba contra theta
theta1=0.6;n=25
(beta=1-sum(dbinom(0:19,n,theta1)))
(theta=seq(0.521,0.955,0.01))
poder=pbinom(19,25,theta)
plot(theta,poder,xlab='Probabilidades',
     ylab='Poder de la Prueba',main='Poder vs Probabilidades',
     col='green',type='l',lwd=2)
abline(h=1-beta,v=0.6,col='tomato')

library(ggplot2)
library(dplyr)
df=tibble(theta,poder)
head(df,20)

ggplot(data=df,mapping=aes(x=theta,y=poder))+
  geom_line(colour='green',linewidth=1.5)+
  labs(x='Probabilidades',y='Poder de la Prueba',
       title='Poder vs Probabilidades')+
  geom_vline(xintercept=0.6,
             color='tomato',lty=2,lwd=0.8)+
  geom_hline(yintercept=1-beta,
             color='tomato',lty=2,lwd=0.8)

###############################################
# Otro ejemplo con mu
# Funcion Potencia. P de H respecto a mu 
###############################################

# Cuando mu se aleja de mu0=2 la potencia es mayor entonces la
# regla de decisión es capaz de reconocer H0 falsas. Cuando n es
# mas grande entonces la potencia de la prueba se acerca
# a 1, una H0 falsa es facil de detectar.
# H0:mu=mu0  vs  H1:mu!=mu0

# Primera Manera
po_norm=function(mu,n,mu0,alpha,sigma){
  1-pnorm((mu0-mu)*sqrt(n)/sigma+qnorm(1-alpha/2))+
    pnorm((mu0-mu)*sqrt(n)/sigma-qnorm(1-alpha/2)) 
}
alpha=0.05
sigma=1
mu0=2
n1=10
n2=30
n3=50

plot(function(x) po_norm(x,n1,mu0,alpha,sigma),0,4,type="l",
     xlab="mu",ylab="funcion potencia",col="tomato",lwd=3)
curve(po_norm(x,n2,mu0,alpha,sigma),0,4,lty=2,add=T,col="green2",lwd=3)
curve(po_norm(x,n3,mu0,alpha,sigma),0,4,lty=2,add=T,col="yellow2",lwd=3)
legend(3,0.5,c("n=10","n=30","n=50"),lty = c(1,2,3),
       col=c("tomato","green2","yellow2"),lwd=3,cex=1.3)

# Segunda Manera
mu=seq(0,4,0.01)
alpha=0.05
sigma=1
mu0=2
n1=10
n2=30
n3=50
mu1=1.6

(beta1=pnorm((mu0-mu1)*sqrt(n1)/sigma+qnorm(1-alpha/2))-
    pnorm((mu0-mu1)*sqrt(n1)/sigma-qnorm(1-alpha/2)))
(beta2=pnorm((mu0-mu1)*sqrt(n2)/sigma+qnorm(1-alpha/2))-
    pnorm((mu0-mu1)*sqrt(n2)/sigma-qnorm(1-alpha/2)))
(beta3=pnorm((mu0-mu1)*sqrt(n3)/sigma+qnorm(1-alpha/2))-
    pnorm((mu0-mu1)*sqrt(n3)/sigma-qnorm(1-alpha/2)))

poder1=1-pnorm((mu0-mu)*sqrt(n1)/sigma+qnorm(1-alpha/2))+
  pnorm((mu0-mu)*sqrt(n1)/sigma-qnorm(1-alpha/2))
poder2=1-pnorm((mu0-mu)*sqrt(n2)/sigma+qnorm(1-alpha/2))+
  pnorm((mu0-mu)*sqrt(n2)/sigma-qnorm(1-alpha/2))
poder3=1-pnorm((mu0-mu)*sqrt(n3)/sigma+qnorm(1-alpha/2))+
  pnorm((mu0-mu)*sqrt(n3)/sigma-qnorm(1-alpha/2))

library(ggplot2)
library(dplyr)
df=tibble(mu,poder1,poder2,poder3)
head(df,20)

ggplot(data=df,mapping=aes(x=mu))+
  geom_line(aes(y=poder1,color='n1=10'),size=1.5)+
  geom_line(aes(y=poder2,color='n2=30'),lty=2,size=1.5)+
  geom_line(aes(y=poder3,color='n3=50'),size=1.5)+
  labs(x='Promedios',y='Poder de la Prueba',
       title='Poder vs Medias')+
  geom_vline(xintercept=mu1,
             color='yellow2',lty=2,lwd=1)+
  geom_hline(yintercept=c(1-beta1,1-beta2,1-beta3),
             color='yellow2',lty=2,lwd=1)+
  theme_dark()+
  scale_color_discrete(name='Tamaño de Muestra')

# Se espera que la funcion potencia tome valores pequeños cuando
# alpha es pequeño
alpha1=0.03
alpha2=0.05
alpha3=0.1
sigma=1
mu0=2
n=20

plot(function(x) po_norm(x,n,mu0,alpha1,sigma),0,4,type="l",
     xlab="mu",ylab="funcion potencia",col="tomato",lwd=3)
curve(po_norm(x,n,mu0,alpha2,sigma),0,4,lty=2,add=T,col="green2",lwd=3)
curve(po_norm(x,n,mu0,alpha3,sigma),0,4,lty=2,add=T,col="yellow2",lwd=3)
legend(2.6,0.5,c("alpha=0.03","alpha=0.05","alpha=0.10"),lty = c(1,2,3),
       col=c("tomato","green2","yellow2"),lwd=3,cex=1.2)

# Segunda Manera

mu=seq(0,4,0.01)
alpha1=0.03
alpha2=0.05
alpha3=0.1
sigma=1
mu0=2
n=20
poder1=1-pnorm((mu0-mu)*sqrt(n)/sigma+qnorm(1-alpha1/2))+
  pnorm((mu0-mu)*sqrt(n)/sigma-qnorm(1-alpha1/2))
poder2=1-pnorm((mu0-mu)*sqrt(n)/sigma+qnorm(1-alpha2/2))+
  pnorm((mu0-mu)*sqrt(n)/sigma-qnorm(1-alpha2/2))
poder3=1-pnorm((mu0-mu)*sqrt(n)/sigma+qnorm(1-alpha3/2))+
  pnorm((mu0-mu)*sqrt(n)/sigma-qnorm(1-alpha3/2))
library(ggplot2)
library(dplyr)
df=tibble(mu,poder1,poder2,poder3)
head(df,20)

ggplot(data=df,mapping=aes(x=mu))+
  geom_line(aes(y=poder1,color='alpha1=0.03'),size=1.5)+
  geom_line(aes(y=poder2,color='alpha2=0.05'),lty=2,size=1.5)+
  geom_line(aes(y=poder3,color='alpha3=0.10'),size=1.5)+
  labs(x='Promedios',y='Poder de la Prueba',
       title='Poder vs Medias')+
  theme_dark()+
  scale_color_discrete(name='Nivel de Significación')

# Funcion Potencia. P de H respecto a sigma^2 

po_sigma=function(sigma,n,sigma0,alpha){
  1-pchisq(sigma0/sigma*qchisq(1-alpha/2,n),n)+
    pchisq(sigma0/sigma*qchisq(alpha/2,n),n)
}

alpha=0.05
sigma0=10
n1=10
n2=30
n3=50

sigma=sigma0+seq(-10,50,0.1)

plot(function(x) po_sigma(x,n1,sigma0,alpha),0,40,type="l",
     xlab="sigma^2",ylab="funcion potencia",col="tomato",lwd=3)
curve(po_sigma(x,n2,sigma0,alpha),0,40,lty=2,add=T,col="green2",lwd=3)
curve(po_sigma(x,n3,sigma0,alpha),0,40,lty=3,add=T,col="yellow3",lwd=3)
legend(30,0.5,c("n=10","n=30","n=50"),lty = c(1,2,3),
       col=c("tomato","green2","yellow2"),lwd=3,cex=1.2)

# Segunda Manera
sigma=seq(0,60,0.01)
alpha=0.05
sigma0=10
n1=10
n2=30
n3=50
poder1=1-pchisq(sigma0/sigma*qchisq(1-alpha/2,n1),n1)+
  pchisq(sigma0/sigma*qchisq(alpha/2,n1),n1)
poder2=1-pchisq(sigma0/sigma*qchisq(1-alpha/2,n2),n2)+
  pchisq(sigma0/sigma*qchisq(alpha/2,n2),n2)
poder3=1-pchisq(sigma0/sigma*qchisq(1-alpha/2,n3),n3)+
  pchisq(sigma0/sigma*qchisq(alpha/2,n3),n3)
library(ggplot2)
library(dplyr)
df=tibble(sigma,poder1,poder2,poder3)
head(df,20)

ggplot(data=df,mapping=aes(x=sigma))+
  geom_line(aes(y=poder1,color='n1=10'),size=1.5)+
  geom_line(aes(y=poder2,color='n2=30'),lty=2,size=1.5)+
  geom_line(aes(y=poder3,color='n3=50'),size=1.5)+
  labs(x='Desviación Estándar',y='Poder de la Prueba',
       title='Poder vs Medias')+
  theme_bw()+
  scale_color_discrete(name='Tamaño de Muestra')

## PRUEBA t CON UNA MUESTRA

## X ~ Normal(mu0,sigma) sigma desconocida.
#   H0:mu = mu0 vs mu != mu0

po_t_NoCen=function(mu,n,mu0,alpha,sigma){
  delta=(mu-mu0)*sqrt(n)/sigma
  1-pt(qt(1-alpha/2,n-1),df=n-1,ncp=delta)+
    pt(qt(alpha/2,n-1),df=n-1,ncp=delta) 
}
alpha=0.05
sigma=1
mu0=2
n1=10
n2=30
n3=50

mu=mu0+seq(-2,2,0.05)

plot(po_t_NoCen(mu,n1,mu0,alpha,sigma),type="l",xaxt='n',
     xlab="mu",ylab="funcion potencia",col="tomato",lwd=3)
axis(1,1:length(mu),mu)
lines(po_t_NoCen(mu,n2,mu0,alpha,sigma),lty=2,col="green2",lwd=3)
lines(po_t_NoCen(mu,n3,mu0,alpha,sigma),lty=2,col="yellow2",lwd=3)
legend(60,0.5,c("n=10","n=30","n=50"),lty = c(1,2,3),
       col=c("tomato","green2","yellow2"),lwd=3,cex=1.3)

# Segunda Manera
mu=seq(0,4,0.01)
alpha=0.05
sigma=1
mu0=2
n1=10
n2=30
n3=50
delta1=(mu-mu0)*sqrt(n1)/sigma
delta2=(mu-mu0)*sqrt(n2)/sigma
delta3=(mu-mu0)*sqrt(n3)/sigma
poder1=1-pt(qt(1-alpha/2,n1-1),df=n1-1,ncp=delta1)+
  pt(qt(alpha/2,n1-1),df=n1-1,ncp=delta1) 
poder2=1-pt(qt(1-alpha/2,n2-1),df=n2-1,ncp=delta2)+
  pt(qt(alpha/2,n2-1),df=n2-1,ncp=delta2)
poder3=1-pt(qt(1-alpha/2,n3-1),df=n3-1,ncp=delta3)+
  pt(qt(alpha/2,n3-1),df=n3-1,ncp=delta3)
library(ggplot2)
library(dplyr)
df=tibble(mu,delta1,delta2,delta3,poder1,poder2,poder3)
head(df,20)

ggplot(data=df,mapping=aes(x=mu))+
  geom_line(aes(y=poder1,color='n1=10'),size=1.5)+
  geom_line(aes(y=poder2,color='n2=30'),lty=2,size=1.5)+
  geom_line(aes(y=poder3,color='n3=50'),size=1.5)+
  labs(x='Promedios',y='Poder de la Prueba',
       title='Poder vs Medias')+
  theme_dark()+
  scale_color_discrete(name='Tamaño de Muestra')

# Ejemplo 17-b: de los tiempos, en segundos, de una terminal de computdora
# en línea. (H0: theta >= 1/5  vs  H1: theta < 1/5)

alpha = 0.05
theta0 = 1/5
n = 5
(c = qgamma(alpha, n, theta0, lower.tail = F))
theta1 = 1/10.2

theta = seq(0, 2, 0.05)
poder = pgamma(c, n, theta, lower.tail = F)
(poder1 = pgamma(c, n, theta1, lower.tail = F))
library(tidyverse)
df=tibble(theta, poder)
head(df,20)

plot(theta, poder, xlab = 'theta', type = 'l',
     ylab = 'Poder de la Prueba', main = 'Poder vs theta',
     col = '#CDAA7D', lwd = 3)
abline(h = poder1, v = theta1, col='#66CDAA', lwd = 2)

ggplot(data = df, mapping = aes(x = theta, y = poder))+
  geom_line(colour = '#CDAA7D', linewidth = 1.5)+
  labs(x = 'theta', y = 'Poder de la Prueba',
       title = 'Poder vs theta') +
  geom_vline(xintercept = theta1,
             color = '#66CDAA', lty = 2, lwd = 0.8)+
  geom_hline(yintercept = poder1,
             color = '#66CDAA', lty = 2, lwd = 0.8)

# Ejemplo 17-e de la guía

theta=seq(0,0.3,0.01)
n=5
alpha=0.05
theta0=1/5
poder1=alpha^(theta/theta0) 
poder2=pchisq(2*theta*qgamma(alpha,n,theta0,lower.tail = F),2*n,lower.tail = F)
library(tidyverse)
df=tibble(theta,poder1,poder2)
head(df,20)

ggplot(data=df,mapping=aes(x=theta))+
  geom_line(aes(y=poder1,color='Mínimo'),linewidth=1.2)+
  geom_line(aes(y=poder2,color='Suma'),lty=2,linewidth=1.2)+
  labs(x='Theta',y='Poder de la Prueba',
       title='Poder vs Theta')+
  theme_dark()+
  scale_color_discrete(name='Funciones Potencia')

# cuando la H0 es falsa (theta < 0.2) se quiere rechazarla con probabilidad alta.
# cuando la H0 es verdadera (theta >= 0.2) se quiere rechazarla con probabilidad baja.
# Utilizando la Suma se obtiene una prueba más poderosa que utilizado el Mínimo.

## Ejemplo de los microbuseros. Determinacion del valor de k.

n=18
x=0:(n)
prob=dbinom(x,n,0.84)
suma_acumulativa=cumsum(prob)

# Valor acumulado es 'menor' o 'mayor' a 0.10
comparacion=ifelse(suma_acumulativa < 0.10, 'Rechazar','No rechazar')

# Tabla
tabla=data.frame(x,prob,suma_acumulativa,comparacion)
names(tabla)=c('x','Probabilidad','Suma Acumulativa','Decision H0')
print(tabla)

(muestra=13)# 13=13(Y=sum(muestra))
a=pbinom(13-1,n,0.84)
b=dbinom(13,n,0.84)
f=function(x) {a+b*x-0.10}
(gamma=uniroot(f,c(0,1))$root)# gamma=0.4820171

# Ejemplo 22: f(x, theta)=(1/8)exp(-(1/8)(x-1/theta))

theta0 = 1/10
n = 20
alpha = 0.05
(k = 1/theta0- 8*log(alpha)/n)
theta1 = 0.092
100/9.2
(theta = seq(1/11.198, 1/8, 0.001))
(poder = exp(-(n/8)*(k-1/theta)))
(poder1 = exp(-(n/8)*(k-1/theta1)))
library(ggplot2)
library(ggthemes)
library(tidyverse)
df=tibble(theta, poder)
head(df,20)

plot(1/theta, poder, xlab = '1/theta', type = 'l',
     ylab = 'Poder de la Prueba', main = 'Poder vs 1/theta',
     col = '#CDAA7D', lwd = 3)
abline(h = poder1, v = 1/theta1, col='#66CDAA', lwd =2)

ggplot(data = df, mapping = aes(x = 1/theta, y = poder))+
  geom_line(colour = '#CDAA7D', size = 1.5)+
  labs(x = '1/theta', y = 'Poder de la Prueba',
       title = 'Poder vs 1/theta') +
  geom_vline(xintercept = 1/theta1,
             color = '#66CDAA', lty = 2, lwd = 0.8)+
  geom_hline(yintercept = poder1,
             color = '#66CDAA', lty = 2, lwd = 0.8)+
  labs(x = '1/Theta', y = 'Poder de la Prueba')

#################################
##  Multinomial con m muestras  #
#################################

multi_m_muestra=function(x){
  # columna j de x debe corresponder a los conteos en la muestra j
  k=dim(x)[1]
  m=dim(x)[2]
  est=matrix(NA,k,m)
  for(j in 1:m){
    est[,j]=x[,j]/colSums(x)[j] ## estimacion Max. Ver.
    p_0=rowSums(x)/(sum(x)) ## estimacion Max. Ver. bajo H0.
    l1=sum(x*log(est))
    l2=sum(rowSums(x)*log(p_0))
    estad=2*(l1-l2)
    p=pchisq(estad,(k-1)*(m-1),lower.tail = F)
  }
  list(estimaciones=est,estadistica=estad,p.valor=p)
}

bajo=c(92,100,53,28)
medio=c(72,89,94,52)
alto=c(84,111,17,10)

Deportes=matrix(c(bajo,medio,alto),
                nrow = 4,
                ncol = 3)
rownames(Deportes)=c("mas_de_3","entre_1_y_3","de_vez_en_cuando","no_practica")
colnames(Deportes)=c("bajo","medio","alto")
Deportes
multi_m_muestra(Deportes)

###### Distribución Asintótica de la Razón de Verosimilitud

## Binomial sin logaritmos y con una salida

bin=function(n,suma,p=0,t=FALSE){  #n->número de elementos para cada marca
  pii=c()                         #suma->número de elementos que cumplen la condici?n                 
  max_div=c()                     #p->si t=TRUE, entonces introducir p para comparar
  
  for(i in 1:length(n)){    #hallamos el p para cada distribuci?n
    pii[i]=suma[i]/n[i] }
  
  if(t==T) {                       #condicional para hallar el p para comparar
    pi=p
  } else {pi=sum(suma)/sum(n)
  }
  
  for(i in 1:length(n)){           #hallamos las probabilidades de cada dist.
    max_div[i]=(pii[i]^suma[i])*((1-pii[i])^(n[i]-suma[i]))} 
  
  max0=((pi^sum(suma)*(1-pi)^(sum(n)-sum(suma))))   
  #hallamos el max[L(o')]
  
  max=prod(max_div)   #hallamos el max[L(o)]
  
  A=max0/max
  
  qu=-2*log(A)           #quantil
  
  if(t==T){           #hallamos los grados de libertad
    df=length(n)
  }else {
    df=length(n)-1
  }
  
  pval=1-pchisq(qu,df)
  
  if(pval<0.05) {
    c("Rechazar la hipotesis nula",pvalor=pval,JiCalc=qu)
  } else {c("No rechazar la hipotesis nula",pvalor=pval,JiCalc=qu)
  }
}

suma=c(10,4,4) 
n=c(85,95,98)
bin(n,suma)

bin(n,suma,0.09,T)

bin(n[c(1,3)],suma[c(1,3)],0.08,T)


## Binomial trabajando con logaritmos y con otra salida

bin2=function(n,suma,p=0,t=FALSE){  #n->n?mero de elementos para cada marca
  pii=c()                         #suma->n?mero de elementos que cumplen la condici?n                 
  lnmax_div=c()                     #p->si t=TRUE, entonces introducir p para comparar
  library(kableExtra)
  
  for(i in 1:length(n)){    #hallamos el p para cada distribuci?n
    pii[i]=suma[i]/n[i] }
  
  if(t==T) {                       #condicional para hallar el p para comparar
    pi=p
  } else {pi=sum(suma)/sum(n)
  }
  
  for(i in 1:length(n)){           #hallamos las probabilidades de cada dist.
    lnmax_div[i]=suma[i]*log(pii[i])+(n[i]-suma[i])*log(1-pii[i])} 
  
  lnmax0=((sum(suma)*log(pi)+(sum(n)-sum(suma))*log((1-pi))))   
  #hallamos el max[L(o')]
  
  max=sum(lnmax_div)   #hallamos el max[L(o)]
  
  A=lnmax0-max
  
  JiCalc=-2*(A)           #quantil
  
  if(t==T){           #hallamos los grados de libertad
    df=length(n)
  }else {
    df=length(n)-1
  }
  
  grados_libertad=df
  JiTabular=qchisq(0.95,df)
  pvalor=1-pchisq(JiCalc,df)
  
  tabl <- data.frame("Grados_Libertad" = grados_libertad,
                     "JiCalcu" = JiCalc,
                     "JiTabular" = JiTabular,
                     "pvalor"=pvalor,
                     row.names = "Resultados")
  tabl %>% kbl() %>% 
    kable_styling(latex_options = "striped")
}
?kable_styling
suma=c(10,4,4) 
n=c(85,95,98)
bin2(n,suma)

bin2(n,suma,0.09,T)

bin2(n[c(1,3)],suma[c(1,3)],0.08,T)

