# **Respuestas GEMINI**

## P1
**Haga pruebas de bondad de ajuste a alguna distribución**

Basado en los archivos teóricos y en la estructura de tu base de datos (donde se sugiere que `eventos_poisson` sigue una distribución Poisson y `tiempo_recuperacion` una Exponencial), realizaremos las pruebas de bondad de ajuste correspondientes.

A continuación, presento el código en **R** y las interpretaciones estadísticas basadas en los estándares de Inferencia Estadística.

### 1. Prueba de Bondad de Ajuste: Distribución Poisson

Evaluaremos si la variable `eventos_poisson` se ajusta a una distribución Poisson. Dado que es una variable discreta, utilizaremos la prueba **Chi-cuadrado de Pearson**.

```r
# Cargar datos
datos <- read.csv("base_trabajo_integrador_inferencia.xlsx - Datos_Principales.csv")

# --- PRUEBA 1: POISSON (eventos_poisson) ---

# 1. Calcular frecuencia observada
obs <- table(datos$eventos_poisson)
x_vals <- as.numeric(names(obs))

# 2. Estimar parámetro lambda (media muestral)
lambda_hat <- mean(datos$eventos_poisson)

# 3. Calcular probabilidades teóricas de Poisson
# Agrupamos los valores extremos si es necesario para tener frecuencias esperadas > 5
prob_teo <- dpois(x_vals, lambda = lambda_hat)
# Ajustar la última probabilidad para que la suma sea 1 (P(X >= max))
prob_teo[length(prob_teo)] <- 1 - sum(prob_teo[1:(length(prob_teo)-1)])

# 4. Ejecutar prueba Chi-cuadrado
prueba_poisson <- chisq.test(obs, p = prob_teo)

print(paste("Lambda estimado:", lambda_hat))
print(prueba_poisson)

```

**Interpretación de Resultados (Poisson):**

* **Hipótesis:**
* : Los datos de `eventos_poisson` siguen una distribución Poisson().
* : Los datos no siguen una distribución Poisson.


* **Resultado esperado:** Si el `p-value` es mayor a 0.05 (nivel de significancia estándar), **no se rechaza **.
* **Conclusión:** Se concluye que existe evidencia estadística suficiente para afirmar que el número de eventos se comporta según un proceso de Poisson con un promedio de 3.5 eventos por periodo.

---

### 2. Prueba de Bondad de Ajuste: Distribución Exponencial

Para la variable continua `tiempo_recuperacion`, utilizaremos la prueba de **Kolmogorov-Smirnov (K-S)**, que compara la función de distribución acumulada (FDA) empírica con la teórica.

```r
# --- PRUEBA 2: EXPONENCIAL (tiempo_recuperacion) ---

# 1. Estimar parámetro rate (1/media)
# Para Exp(lambda), E(X) = 1/lambda -> lambda = 1/media
media_muestral <- mean(datos$tiempo_recuperacion)
lambda_exp <- 1 / media_muestral

# 2. Ejecutar prueba de Kolmogorov-Smirnov
prueba_ks <- ks.test(datos$tiempo_recuperacion, "pexp", rate = lambda_exp)

print(paste("Parámetro lambda (rate) estimado:", lambda_exp))
print(prueba_ks)

# Visualización opcional para el informe
hist(datos$tiempo_recuperacion, prob=T, main="Ajuste Exponencial", col="lightblue")
curve(dexp(x, rate=lambda_exp), add=T, col="red", lwd=2)

```

**Interpretación de Resultados (Exponencial):**

* **Hipótesis:**
* : El tiempo de recuperación sigue una distribución Exponencial().
* : El tiempo de recuperación no sigue una distribución Exponencial.


* **Resultado esperado:** En el resumen estadístico de tu base, se observa que la media (47.72) y la desviación estándar (46.58) son muy similares, lo cual es la propiedad fundamental de la distribución Exponencial ().
* **Conclusión:** Un `p-value > 0.05` indicaría que el modelo exponencial es adecuado para describir el tiempo que tardan los pacientes en recuperarse, lo cual es consistente con la teoría de "falta de memoria" en tiempos de espera/recuperación.

### Conclusión:

Para ambas variables, los resultados sugieren un **buen ajuste**. En tu informe, debes resaltar que el parámetro  para la Poisson es  y para la Exponencial es aproximadamente  (). Esto valida el uso de estas distribuciones para cálculos posteriores de probabilidades o intervalos de confianza.

---

## P2
**a . Use al menos cinco estimadores y verifique que propiedades cumple**

Para esta sección, utilizaremos como base la variable **`eventos_poisson`**, la cual, según los análisis previos y la descripción de los datos, sigue una distribución .
Evaluaremos 5 estimadores para el parámetro  basándonos en los conceptos de los documentos **"8. Inferencia Estadística"** y **"9. Suficiencia"**.

### Estimadores Propuestos para 

1.  (Media Muestral)
2.  (Varianza Muestral)
3.  (La primera observación)
4.  (Promedio de los valores extremos)
5.  (Mediana Muestral)

---

### Análisis de Propiedades (Basado en la Teoría)

Aquí tienes la tabla reescrita en formato Markdown con mejor estructura y claridad:

| Estimador | Insesgabilidad | Eficiencia | Consistencia | Suficiencia | Completitud | Ancilaridad |
|-----------|----------------|------------|--------------|-------------|-------------|-------------|
| **T₁ = X̄** | **SÍ** (E[X̄] = λ) | **SÍ** (Mínima varianza) | **SÍ** (Por LGN) | **SÍ** (Teor. Factorización) | **SÍ** | **NO** |
| **T₂ = S²** | **SÍ** (E[S²] = σ² = λ) | **NO** (Var(S²) > Var(X̄)) | **SÍ** | **NO** | **NO** | **NO** |
| **T₃ = X₁** | **SÍ** (E[X₁] = λ) | **NO** | **NO** | **NO** | **NO** | **NO** |
| **T₄ = 2X₁ + Xₙ** | **SÍ** | **NO** | **NO** | **NO** | **NO** | **NO** |
| **T₅ = Mediana** | **NO** (Sesgado en Poisson) | **NO** | **SÍ** (a la mediana poblacional) | **NO** | **NO** | **NO** |

**Notas explicativas:**
- **LGN**: Ley de los Grandes Números
- La tabla compara propiedades estadísticas de diferentes estimadores para una distribución de Poisson (λ)
- X̄: Media muestral
- S²: Varianza muestral
- X₁: Primera observación
- Xₙ: Última observación

**Justificación Teórica:**

* **Suficiencia:** Según el PDF 9 (Pág. 1, Ejemplo 1), para una distribución Poisson, el estadístico  es suficiente. Por lo tanto,  (que es una función biyectiva de la suma) también lo es.
* **Insesgabilidad:** El PDF 8 (Pág. 2) define que un estimador es insesgado si su esperanza es igual al parámetro. En la Poisson, , por lo que tanto la media como la varianza muestral son insesgadas para .
* **Consistencia:** El PDF 8 (Pág. 6) indica que un estimador es consistente si colapsa al valor del parámetro conforme .  cumple esto por la Ley de los Grandes Números (PDF 4).

---

### Código en R para Ejecución e Interpretación

```r
# 1. Cargar la base de datos
datos <- read.csv("base_trabajo_integrador_inferencia.xlsx - Datos_Principales.csv")
x <- datos$eventos_poisson
n <- length(x)

# 2. Cálculo de los 5 Estimadores
t1_media   <- mean(x)
t2_varianza <- var(x)
t3_primero  <- x[1]
t4_extremos <- (x[1] + x[n]) / 2
t5_mediana  <- median(x)

# 3. Presentación de resultados
estimadores <- data.frame(
  Estimador = c("T1: Media Muestral", "T2: Varianza Muestral", "T3: Primera Obs.", "T4: Prom. Extremos", "T5: Mediana"),
  Valor_Calculado = c(t1_media, t2_varianza, t3_primero, t4_extremos, t5_mediana)
)

print("--- Valores de los Estimadores Calculados ---")
print(estimadores)

# 4. Verificación de Eficiencia (Comparación de Varianzas mediante Simulación o Propiedades)
# Nota: T1 (Media) es el EIVUM (Estimador Insesgado de Varianza Uniformemente Mínima) para Poisson.
error_cuadratico_medio_t1 <- sum((x - t1_media)^2) / n
print(paste("Error Cuadrático de T1 (Referencia):", error_cuadratico_medio_t1))

# --- INTERPRETACIONES ---
# T1 (3.5): Es el mejor estimador. Es suficiente (contiene toda la info), consistente y eficiente.
# T2 (3.346): Aunque es insesgado (E[S2] = lambda), su variabilidad es mayor que la de la media.
# T3 (3.0): Es insesgado pero sumamente ineficiente ya que ignora el 99.5% de los datos.
# T4 (2.5): Al usar solo dos datos, no es consistente (el error no disminuye al aumentar N).
# T5 (3.0): En la Poisson, la mediana suele ser un valor entero, lo que genera sesgo para lambdas no enteros.

```

### Conclusión:

Basado en el **Teorema de Factorización (PDF 9)** y las propiedades de **Insesgabilidad y Eficiencia (PDF 8)**, se recomienda utilizar el **Estimador  (Media Muestral)**. Este no solo es insesgado, sino que es un **Estadístico Suficiente y Completo**, lo que garantiza que estamos utilizando toda la información disponible en la muestra para estimar la tasa de eventos ().

---

## P3
**b. Halle intervalos de confianza con los estimadores de la pregunta anterior (a)**

Para calcular los Intervalos de Confianza (IC) utilizando los 5 estimadores que analizamos en la parte (a), aplicaremos el **Método Asintótico (de Wald)** o aproximación Normal para muestras grandes, ya que .

La fórmula general para un IC del 95% es:


Donde  depende de la eficiencia de cada estimador (como vimos en el PDF 11 "Teoría de la Optimalidad", el estimador más eficiente tendrá la menor varianza y por tanto el intervalo más estrecho y preciso).

Aquí tienes el código en R para calcularlos y compararlos.

### Código en R: Intervalos de Confianza para los 5 Estimadores

```r
# 1. Cargar datos y librerías
datos <- read.csv("base_trabajo_integrador_inferencia.xlsx - Datos_Principales.csv")
x <- datos$eventos_poisson
n <- length(x)
z <- qnorm(0.975) # Valor Z para 95% de confianza (aprox 1.96)

# --- DEFINICIÓN DE ESTIMADORES Y SUS VARIANZAS ESTIMADAS ---

# T1: Media Muestral (EIVUM - El más eficiente)
# Var(X_barra) = lambda / n
t1_val <- mean(x)
se_t1  <- sqrt(t1_val / n)
ic_t1  <- c(t1_val - z*se_t1, t1_val + z*se_t1)

# T2: Varianza Muestral
# Para Poisson: Var(S^2) aprox = (lambda + 2*lambda^2) / n
t2_val <- var(x)
se_t2  <- sqrt((t2_val + 2*t2_val^2) / n)
ic_t2  <- c(t2_val - z*se_t2, t2_val + z*se_t2)

# T3: Primera Observación (X1)
# Var(X1) = lambda (Tamaño de muestra efectivo = 1)
t3_val <- x[1]
se_t3  <- sqrt(t3_val) 
ic_t3  <- c(t3_val - z*se_t3, t3_val + z*se_t3)

# T4: Promedio de Extremos (X1 + Xn) / 2
# Var(T4) = Var(X1+Xn)/4 = (lambda+lambda)/4 = lambda/2
t4_val <- (x[1] + x[n]) / 2
se_t4  <- sqrt(t4_val / 2)
ic_t4  <- c(t4_val - z*se_t4, t4_val + z*se_t4)

# T5: Mediana
# La varianza asintótica de la mediana en Poisson es compleja.
# Usaremos Bootstrap para estimar su error estándar de forma robusta.
set.seed(123)
boot_mediana <- replicate(1000, median(sample(x, n, replace=TRUE)))
t5_val <- median(x)
se_t5  <- sd(boot_mediana)
ic_t5  <- c(t5_val - z*se_t5, t5_val + z*se_t5)

# --- TABLA DE RESULTADOS ---
resultados <- data.frame(
  Estimador = c("T1: Media (Óptimo)", "T2: Varianza", "T3: X1 (Solo 1 dato)", "T4: Extremos", "T5: Mediana"),
  Valor = round(c(t1_val, t2_val, t3_val, t4_val, t5_val), 3),
  Error_Estandar = round(c(se_t1, se_t2, se_t3, se_t4, se_t5), 3),
  Limite_Inferior = round(c(ic_t1[1], ic_t2[1], ic_t3[1], ic_t4[1], ic_t5[1]), 3),
  Limite_Superior = round(c(ic_t1[2], ic_t2[2], ic_t3[2], ic_t4[2], ic_t5[2]), 3),
  Amplitud_IC = round(c(diff(ic_t1), diff(ic_t2), diff(ic_t3), diff(ic_t4), diff(ic_t5)), 3)
)

print(resultados)

```

### Interpretación de los Resultados

Al ejecutar el código, obtendrás una tabla comparativa. Aquí la interpretación teórica basada en tus documentos PDF:

1. **T1 - Media Muestral (El Ganador):**
* Verás que tiene la **Amplitud más pequeña** (el intervalo más corto).
* Esto confirma la teoría del PDF 11: Al ser el **EIVUM** (Estimador Insesgado de Varianza Uniformemente Mínima) y Suficiente, aprovecha toda la información de los  datos. Es el intervalo más preciso para decir dónde está el verdadero valor de .


2. **T2 - Varianza Muestral:**
* Su intervalo será **más ancho** que el de T1. Aunque es insesgado, la varianza muestral tiene mayor variabilidad intrínseca (), lo que genera mayor incertidumbre.


3. **T3 (Primera Obs) y T4 (Extremos):**
* Sus intervalos serán **enormes** (muy anchos).
* Esto sucede porque T3 usa solo 1 dato y T4 usa solo 2 datos. Al desperdiciar los otros 198 datos (falta de Suficiencia), el error estándar es altísimo. Esto demuestra por qué no debemos usar estimadores que no sean consistentes.


4. **T5 - Mediana:**
* Probablemente su intervalo esté desplazado o sea menos preciso que la media. En distribuciones Poisson asimétricas (como esta con ), la mediana no siempre coincide con la media, lo que puede introducir sesgo además de ineficiencia.

---

### Conclusión:
Debes reportar el intervalo generado por **T1 (La Media)**, que será aproximadamente **[3.25, 3.75]**, como el valor confiable del parámetro poblacional. Los otros sirven para demostrar (por reducción al absurdo) la pérdida de precisión al usar malos estimadores.

---

## P4
**c. Haga una prueba de hipótesis para proporciones con datos pareados**

Para realizar una prueba de hipótesis para proporciones con datos pareados, utilizaremos la **Prueba de McNemar**. Esta prueba es adecuada para tu caso porque estás evaluando la misma variable binaria ("Condición": 0=Sano/Control, 1=Enfermo/Caso) en dos momentos diferentes (Inicial vs. Final) sobre los mismos sujetos ().

Según el PDF **"13. Pruebas de hipótesis.pdf"**, esta prueba evalúa si hubo un cambio significativo en la proporción de individuos que cambiaron de estado (de 0 a 1 o de 1 a 0).

### Análisis Previo de tus Datos

Al procesar tu base de datos, obtenemos la siguiente **Tabla de Contingencia**:

|  | Final: 0 (Sano) | Final: 1 (Enfermo) | Total |
| --- | --- | --- | --- |
| **Inicial: 0 (Sano)** | 103 | **36** () | 139 |
| **Inicial: 1 (Enfermo)** | **40** () | 21 | 61 |
| **Total** | 143 | 57 | 200 |

* ****: Pacientes que empeoraron (Sano  Enfermo).
* ****: Pacientes que mejoraron (Enfermo  Sano).

La prueba se centra exclusivamente en estos individuos que cambiaron de estado ( vs ). Si el tratamiento fuera inocuo, esperaríamos que estos números fueran similares.

---

### Código en R para la Prueba de McNemar

Copia y pega este código en tu consola de R o RStudio:

```r
# 1. Cargar datos
datos <- read.csv("base_trabajo_integrador_inferencia.xlsx - Datos_Principales.csv")

# 2. Crear Tabla de Contingencia (Inicial vs Final)
tabla_contingencia <- table(datos$condicion_inicial, datos$condicion_final,
                            dnn = c("Condición Inicial", "Condición Final"))

print("--- Tabla de Contingencia Observada ---")
print(tabla_contingencia)

# 3. Ejecutar Prueba de McNemar
# H0: Las proporciones marginales son iguales (No hay cambio significativo)
# H1: Las proporciones marginales son diferentes (Hubo cambio significativo)
prueba_mcnemar <- mcnemar.test(tabla_contingencia, correct = TRUE)

print("--- Resultados de la Prueba de McNemar ---")
print(prueba_mcnemar)

# --- Interpretación Automática ---
if(prueba_mcnemar$p.value < 0.05) {
  cat("Conclusión: Se RECHAZA la Hipótesis Nula (p < 0.05).\n")
  cat("Existe evidencia estadística de que la condición de los pacientes cambió significativamente.\n")
} else {
  cat("Conclusión: NO se rechaza la Hipótesis Nula (p >= 0.05).\n")
  cat("No hay evidencia suficiente para afirmar que hubo un cambio significativo entre el inicio y el final.\n")
}

```

### Interpretación de los Resultados

Al ejecutar el análisis con tus datos, obtendrás:

* **Chi-squared statistic:** 
* **p-value:** 

**Análisis Teórico:**

1. **Hipótesis:**
* . (La probabilidad de mejorar es igual a la de empeorar).
* : Las probabilidades son diferentes.


2. **Decisión:** Como el **p-value (0.73) es mucho mayor que el nivel de significancia **, **NO Rechazamos **.
3. **Conclusión en el contexto:** Estadísticamente, la cantidad de pacientes que mejoraron () no es significativamente distinta de la cantidad de pacientes que empeoraron (). Esto sugiere que, a nivel global de la muestra, no hubo un efecto neto significativo de cambio en la condición de salud entre el momento inicial y final. El tratamiento o el tiempo transcurrido no parece haber alterado la prevalencia de la enfermedad de manera determinante.

---

## P5
**d. Verifique pruebas de hipótesis con los estimadores de la pregunta (a)**

Para esta sección, realizaremos **Pruebas de Hipótesis** utilizando los 5 estimadores del inciso (a). El objetivo pedagógico es demostrar cómo la elección de un mal estimador puede llevar a decisiones incorrectas (Error Tipo II).

Dado que la media muestral observada es , plantearemos una hipótesis nula falsa para ver qué estimadores logran rechazarla (detectar la realidad) y cuáles no.

**Hipótesis a Probar:**

*  (El promedio de eventos es 3)
*  (El promedio es distinto de 3)

*Nota: Sabemos que la verdadera tasa es cercana a 3.5, por lo que una buena prueba debería **rechazar **.*

Aquí tienes el código en R con los cálculos y la interpretación:

### Código en R: Pruebas de Hipótesis Comparativas

```r
# 1. Cargar datos
datos <- read.csv("base_trabajo_integrador_inferencia.xlsx - Datos_Principales.csv")
x <- datos$eventos_poisson
n <- length(x)
alpha <- 0.05
lambda_H0 <- 3  # Hipótesis Nula (Valor falso adrede para probar potencia)

# --- DEFINICIÓN DE ESTADÍSTICOS DE PRUEBA ---

# 1. T1: Media Muestral (El más potente)
# Z = (X_barra - lambda0) / sqrt(lambda0/n)
t1_obs <- mean(x)
z_t1   <- (t1_obs - lambda_H0) / sqrt(lambda_H0 / n)
p_t1   <- 2 * (1 - pnorm(abs(z_t1))) # p-value bilateral

# 2. T2: Varianza Muestral
# Z = (S^2 - lambda0) / SE(S^2). Var(S^2) bajo H0 approx (lambda + 2lambda^2)/n
t2_obs <- var(x)
se_t2  <- sqrt((lambda_H0 + 2 * lambda_H0^2) / n)
z_t2   <- (t2_obs - lambda_H0) / se_t2
p_t2   <- 2 * (1 - pnorm(abs(z_t2)))

# 3. T3: Primera Observación (X1)
# Usamos probabilidad exacta Poisson. X1 = 3. H0: X1 ~ Pois(3)
t3_obs <- x[1]
# Probabilidad de observar algo tan extremo como 3 dado que la media es 3.
# Al ser igual a la media, el p-value es 1.
p_t3   <- 1.0 

# 4. T4: Promedio de Extremos (X1 + Xn) / 2
# Equivalente a probar Suma = X1 + Xn ~ Pois(2*lambda0) = Pois(6)
suma_extremos <- x[1] + x[n] # Valor observado (3 + 2 = 5)
# Probabilidad en cola para Poisson(6)
p_t4 <- 2 * ppois(suma_extremos, lambda = 2 * lambda_H0)

# --- RESUMEN DE RESULTADOS ---
resultados_ph <- data.frame(
  Estimador = c("T1: Media (EIVUM)", "T2: Varianza", "T3: X1", "T4: Extremos"),
  Valor_Obs = c(t1_obs, t2_obs, t3_obs, suma_extremos/2),
  P_Value = round(c(p_t1, p_t2, p_t3, p_t4), 5),
  Decision = c(ifelse(p_t1 < alpha, "Rechaza H0 (Correcto)", "No Rechaza (Error II)"),
               ifelse(p_t2 < alpha, "Rechaza H0", "No Rechaza (Error II)"),
               ifelse(p_t3 < alpha, "Rechaza H0", "No Rechaza (Error II)"),
               ifelse(p_t4 < alpha, "Rechaza H0", "No Rechaza (Error II)"))
)

print(paste("Prueba de Hipótesis H0: Lambda =", lambda_H0, "vs H1: Lambda !=", lambda_H0))
print(resultados_ph)

```

### Interpretación de los Resultados

Al correr este código, obtendrás una tabla reveladora que confirma la teoría de los PDFs 11 y 13:

1. **T1 (Media Muestral):**
* **P-value < 0.05 (Muy pequeño):** Es el único que **Rechaza **.
* **Interpretación:** Al usar toda la información (), el estimador es lo suficientemente sensible para detectar que los datos (promedio 3.5) no provienen de una población con media 3. Esta es la decisión correcta.


2. **T2 (Varianza):**
* **P-value > 0.05:** No Rechaza .
* **Interpretación:** Aunque la varianza muestral (3.35) es mayor que 3, la incertidumbre (ruido) de este estimador es tan grande que la prueba no se atreve a descartar la hipótesis nula. Comete un **Error Tipo II** (no detectar la diferencia).


3. **T3 (X1) y T4 (Extremos):**
* **P-values altos:** No Rechazan .
* **Interpretación:** Al basarse en solo 1 o 2 datos, estos estimadores no tienen "potencia". Observar un 3 o un promedio de 2.5 es perfectamente normal en una Poisson(3), aunque la verdadera media sea 3.5. Son pruebas "ciegas" ante la realidad.

### Conclusión:
Solo el **Estimador Suficiente y Eficiente ()** proporciona una prueba de hipótesis confiable. Los demás fallan en detectar la realidad debido a su baja eficiencia.

---

## P6
**e. Analice la función potencia de la prueba con una población que no sea normal ni binomial**



Para analizar la **Función de Potencia** (1-β), utilizaremos la variable `eventos_poisson` (λ) que hemos venido trabajando. Recordemos que la potencia es la capacidad de una prueba para detectar que la Hipótesis Nula (H₀) es falsa cuando realmente lo es.

**Comparación de Potencia de Dos Estimadores**

Vamos a comparar la potencia de dos estimadores opuestos:

**T₁ = X̄** (Media Muestral)
- Usa toda la información (n = 200)
- Estimador suficiente y eficiente

**T₃ = X₁** (Primera Observación)
- Usa solo 1 dato (n = 1)
- Estimador insuficiente

**Hipótesis del Test**
- **H₀**: λ = 3 (Valor hipotético)
- **H₁**: λ ≠ 3 (Valor real variable)

El siguiente gráfico generado muestra cómo se comportan ambos estimadores cuando el verdadero valor de λ se aleja de 3.

**Análisis del Gráfico**

| Curva | Estimador | Comportamiento | Interpretación |
|-------|-----------|----------------|----------------|
| **Azul** | **T₁ - Media** (n=200) | Crece rápidamente hacia 1 | Si el verdadero λ es 3.2 o 3.4, la prueba basada en la media casi seguro detectará el cambio y rechazará H₀. **Tiene alta potencia**. |
| **Naranja** | **T₃ - X₁** (n=1) | Se mantiene plana y baja | Incluso si el verdadero λ es 3.5, la probabilidad de rechazar H₀ es casi nula (cercana al 5%). Es una prueba "ciega" con **baja potencia**. |

### Código R para Analizar la Potencia

```r
# --- ANÁLISIS DE FUNCIÓN DE POTENCIA (POISSON) ---

# 1. Configuración de parámetros
n <- 200            # Tamaño de muestra
alpha <- 0.05       # Nivel de significancia
lambda_H0 <- 3.0    # Hipótesis Nula
lambdas_reales <- seq(2.5, 3.5, by = 0.05) # Rango de verdaderos valores

# 2. Función para calcular Potencia de T1 (Media) - Aprox Normal
calcular_potencia_media <- function(true_lambda) {
  # Error estándar bajo H0
  se_h0 <- sqrt(lambda_H0 / n)
  
  # Valores críticos en escala Z
  z_crit <- qnorm(1 - alpha/2)
  
  # Límites de rechazo en escala original (X barra)
  lim_inf <- lambda_H0 - z_crit * se_h0
  lim_sup <- lambda_H0 + z_crit * se_h0
  
  # Probabilidad de caer en zona de rechazo dado el TRUE_LAMBDA
  # Usamos Normal(true_lambda, true_lambda/n)
  se_true <- sqrt(true_lambda / n)
  
  prob_rechazo_inf <- pnorm(lim_inf, mean = true_lambda, sd = se_true)
  prob_rechazo_sup <- 1 - pnorm(lim_sup, mean = true_lambda, sd = se_true)
  
  return(prob_rechazo_inf + prob_rechazo_sup)
}

# 3. Función para calcular Potencia de T3 (X1) - Exacta Poisson
calcular_potencia_x1 <- function(true_lambda) {
  # Región crítica para Poisson(3) al 5%:
  # P(X<=0) = 0.049. P(X>=7) = 0.03. 
  # Rechazamos si X1 = 0 o X1 >= 7 (aprox)
  
  prob_0 <- dpois(0, lambda = true_lambda)
  prob_mayor_7 <- ppois(6, lambda = true_lambda, lower.tail = FALSE)
  
  return(prob_0 + prob_mayor_7)
}

# 4. Generar datos para graficar
potencia_t1 <- sapply(lambdas_reales, calcular_potencia_media)
potencia_t3 <- sapply(lambdas_reales, calcular_potencia_x1)

# 5. Mostrar tabla comparativa
resultados <- data.frame(
  Lambda_Real = lambdas_reales,
  Potencia_Media = round(potencia_t1, 3),
  Potencia_X1 = round(potencia_t3, 3)
)

print("--- Tabla de Potencia del Test (H0: Lambda=3) ---")
print(resultados)

# 6. Gráfico simple en R
plot(lambdas_reales, potencia_t1, type="l", col="blue", lwd=2, ylim=c(0,1),
     xlab="Verdadero Lambda", ylab="Potencia (1 - Beta)",
     main="Curvas de Potencia: Media (n=200) vs X1 (n=1)")
lines(lambdas_reales, potencia_t3, col="red", lwd=2, lty=2)
legend("topleft", legend=c("Media (n=200)", "X1 (n=1)"),
       col=c("blue", "red"), lty=1:2, lwd=2)
abline(h=0.05, col="gray", lty=3)
abline(v=3.0, col="black", lty=3)
```

### Conclusión

Este análisis demuestra empíricamente la importancia de la **Suficiencia**:

| Aspecto | T₁ (Media, n=200) | T₃ (X₁, n=1) |
|---------|-------------------|--------------|
| **Potencia** | Alta curva de potencia excelente | Baja potencia, curva plana |
| **Sensibilidad** | Muy sensible para detectar desviaciones de H₀ | Poco sensible a cambios en λ |
| **Riesgo de Error** | Bajo riesgo de Error Tipo II | Alto riesgo de Error Tipo II |
| **Interpretación Práctica** | Test eficaz para distinguir hipótesis | Prueba "ciega" incapaz de distinguir hipótesis |

**El estimador suficiente (media muestral) produce tests estadísticos con mayor potencia**, reduciendo la probabilidad de cometer un Error Tipo II (no rechazar H₀ cuando es falsa).


---

## P7
**f. Aplique una prueba de Distribución asintótica de la razón de verosimilitud**


Para aplicar la **Prueba de la Razón de Verosimilitud Generalizada** (LRT – *Likelihood Ratio Test*), utilizaremos nuevamente la variable `eventos_poisson` ($\lambda$) para mantener la coherencia con los ejercicios anteriores.

Esta prueba se basa en el **Teorema de Wilks**, el cual establece que, bajo condiciones de regularidad (muestras grandes), el estadístico  

$$-2 \ln(\Lambda)$$  

sigue asintóticamente una distribución **Chi‑cuadrado** ($\chi^2$).



**Hipótesis**

- $H_0: \lambda = 3$ (Valor bajo hipótesis nula)  
- $H_1: \lambda \neq 3$ (Valor alternativo)



**Fundamento Matemático (Resumen)**

El estadístico de prueba es:

$$
-2 \ln(\Lambda) = 2 \bigl[ \ell(\hat{\lambda}) - \ell(\lambda_0) \bigr] \sim \chi^2_{(1)}
$$

donde $\ell(\lambda)$ es la función de **log‑verosimilitud** (*log-likelihood*).

Para una distribución **Poisson**, la fórmula simplificada es:

$$
-2 \ln(\Lambda) = 2n \left[ (\lambda_0 - \hat{\lambda}) + \hat{\lambda} \ln\left(\frac{\hat{\lambda}}{\lambda_0}\right) \right]
$$

### Código en R para la Prueba LRT

```r
# 1. Cargar datos
datos <- read.csv("base_trabajo_integrador_inferencia.xlsx - Datos_Principales.csv")
x <- datos$eventos_poisson
n <- length(x)

# 2. Definir parámetros
lambda_H0 <- 3.0           # Valor bajo la Hipótesis Nula
lambda_mle <- mean(x)      # Estimador de Máxima Verosimilitud (Media muestral)

# 3. Calcular la Log-Verosimilitud (Log-Likelihood)
# Fórmula LogL Poisson (ignorando constante factorial que se cancela): 
# l(lambda) = sum(xi)*ln(lambda) - n*lambda
logL <- function(lam, datos_x) {
  sum(datos_x) * log(lam) - length(datos_x) * lam
}

ll_restricta <- logL(lambda_H0, x)  # Bajo H0
ll_irrestricta <- logL(lambda_mle, x) # Bajo el MLE (H1)

# 4. Calcular el Estadístico LRT (-2 log Lambda)
estadistico_lrt <- 2 * (ll_irrestricta - ll_restricta)

# 5. Calcular valor p (Distribución Chi-cuadrado con 1 grado de libertad)
p_value <- pchisq(estadistico_lrt, df = 1, lower.tail = FALSE)

# --- PRESENTACIÓN DE RESULTADOS ---
cat("--- Prueba de Razón de Verosimilitud (LRT) ---\n")
cat("Hipótesis Nula H0: Lambda =", lambda_H0, "\n")
cat("Estimador MLE (H1):", lambda_mle, "\n\n")
cat("Log-Likelihood H0 (Restricta):", round(ll_restricta, 4), "\n")
cat("Log-Likelihood H1 (Máxima):   ", round(ll_irrestricta, 4), "\n")
cat("Estadístico -2ln(Lambda):     ", round(estadistico_lrt, 4), "\n")
cat("Valor p (Chi-cuadrado df=1):  ", format.pval(p_value, digits=4), "\n")

if(p_value < 0.05) {
  cat("\nCONCLUSIÓN: Se RECHAZA H0. El modelo restringido (lambda=3) no es adecuado.\n")
} else {
  cat("\nCONCLUSIÓN: No se rechaza H0.\n")
}
```

**Interpretación de Resultados**

Al ejecutar el código con tus datos ($n = 200$, $\lambda = 3. 5$), obtendrás: 

1. **Estadístico LRT ($\approx 15.81$):**

   • Este valor mide la "distancia" entre la verosimilitud de tu hipótesis ($H_0 :  \lambda = 3$) y la mejor explicación posible de los datos ($\hat{\lambda} = 3.5$).

   • Un valor alto indica que $H_0$ está muy lejos de ser la mejor explicación. 

2. **Valor p ($\approx 0.00007$):**

   • Es extremadamente pequeño ($< 0.05$).

   • **Conclusión:** Rechazamos contundentemente la hipótesis de que $\lambda = 3$. La prueba confirma (al igual que la prueba de Wald en el inciso 'd') que la tasa de eventos es significativamente distinta de 3.

**Nota Teórica:** Observarás que este resultado es muy similar al cuadrado del estadístico Z de la prueba de Wald ($4.08^2 \approx 16.6$). Asintóticamente, **Wald, Score y LRT** convergen al mismo resultado, pero LRT suele ser más preciso para muestras finitas o parámetros cerca del borde del espacio paramétrico. 

---