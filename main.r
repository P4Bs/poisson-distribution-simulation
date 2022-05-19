## EJERCICIO 1 - GENERACION DE UNA MUESTRA DE TAMANYO N

### VARIABLES ###
sample_size <- 100000
lambda <- 7 ## PROVISIONAL !!!!
## simulation_values <- integer()
simulation_values <- new.env()


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
# for (i in 1:sample_size) simulation_values[i] <- gen_poisson_values(lambda)
for (i in 1:sample_size) {
  value <- gen_poisson_values(lambda)
  key <- as.character(value)
  if (is.null(simulation_values[[key]])) {
    simulation_values[[key]] <- 0
  }
  simulation_values[[key]] <- simulation_values[[key]] + 1
}

get_max_min_values <- function(sim_vals) {
  vals <- as.integer(ls(simulation_values))
  return(c(min(vals), max(vals)))
}

min_max <- get_max_min_values(simulation_values)


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