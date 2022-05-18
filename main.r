## EJERCICIO 1 - GENERACION DE UNA MUESTRA DE TAMANYO N

### VARIABLES ###
sample_size <- 10000
lambda <- 2 ## PROVISIONAL !!!!
simulation_values <- vector()

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
max_value <- simulation_values[which.max(simulation_values)]
min_value <- simulation_values[which.min(simulation_values)]


## EJERCICIO 2 - HISTOGRAMA DE LA DISTRIBUCION DE FRECUENCIAS ##
## GENERAMOS NUESTRO HISTOGRAMA PARA LA DISTRIBUCION DE FRECUENCIAS ##
hist(simulation_values,
     main = paste("Poisson Distribution Simulation", sample_size,
                  "values, lambda =", lambda),
     xlab = "Generated Values",
     xlim = c(min_value, max_value),
     breaks = max_value,
     col = "#79cc5b"
     )


## EJERCICIO 3 - ESTIMACION DE PROBABILIDADES DE INTERVALOS ##
