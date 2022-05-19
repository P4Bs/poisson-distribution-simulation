## EJERCICIO 1 - GENERACION DE UNA MUESTRA DE TAMANYO N

### VARIABLES ###
sample_size <- 100000
lambda <- 19 ## PROVISIONAL !!!!
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

frequency <- as.data.frame(table(simulation_values))
min_val
max_val <- frequency$simulation_values[nrow(frequency)]



## EJERCICIO 2 - HIS  TOGRAMA DE LA DISTRIBUCION DE FRECUENCIAS ##
## GENERAMOS NUESTRO HISTOGRAMA PARA LA DISTRIBUCION DE FRECUENCIAS ##
hist(simulation_values,
     xlab = "Generated Values",
     col = "#79cc5b"
     )

## EJERCICIO 3 - ESTIMACION DE PROBABILIDADES DE INTERVALOS ##