## EJERCICIO 1 - GENERACION DE UNA MUESTRA DE TAMANYO N

### VARIABLES ###
sample_size <- 100000
lambda <- 109 ## PROVISIONAL !!!!
simulation_values <- integer()

## FUNCION PARA GENERAR NUESTROS VALORES DE LA DISTRIBUCION POISSON ##
gen_poisson_values <- function(lambda) {
  sum <- 0
  count <- 0
  while (sum < lambda) {
    sum <- sum + -log(runif(1))
    count <- count + 1
  }
  return(count)
}

## GENERANDO NUESTOR VECTOR DE VALORES ##
for (i in 1:sample_size) simulation_values[i] <- gen_poisson_values(lambda)
min_val <- min(simulation_values)
max_val <- max(simulation_values)

## EJERCICIO 2 - HIS  TOGRAMA DE LA DISTRIBUCION DE FRECUENCIAS ##
## GENERAMOS NUESTRO HISTOGRAMA PARA LA DISTRIBUCION DE FRECUENCIAS ##
hist(simulation_values,
      main = paste("Poisson Distribution Simulation", sample_size,
                    "values, lambda =", lambda),
      xlab = "Generated Values",
      xlim = c(min_val, max_val),
      breaks = max_val - min_val,
      col = "#79cc5b"
     )

## EJERCICIO 3 - ESTIMACION DE PROBABILIDADES DE INTERVALOS ##


## EJERCICIO 4 - VALORES PUNTUALES DE LA MEDIA Y LA VARIANZA MUESTRAL 
sample_mean <- mean(simulation_values)
sample_var <- var(simulation_values)

## EJERICICIO 5 - APLICACION DEL TEOREMA CENTRAL DEL LIMITE 

clt_function <- function(x) {
  num_aux <- (x - sample_mean) / sqrt(sample_var)
  return(num_aux)
}

clt_values <- sapply(simulation_values, clt_function)
clt_mean <- mean(clt_values)
clt_var <- var(clt_values)


hist(clt_values,
     xlab = "Generated Values",
     col = "#79cc5b"
)

sample_normal <- rnorm(100000)
hist(sample_normal,
      xlab = "Generated Values",
      col = "pink"
)

par(mfrow = c(1, 2)) #cambiar a (1,2) para sacar la superposicion de graficas
