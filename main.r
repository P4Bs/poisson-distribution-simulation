## NUESTRA SEMILLA ##
set.seed(42069) ## jaja numero gracioso

## EJERCICIO 1 - GENERACION DE UNA MUESTRA DE TAMANYO N
### VARIABLES ###
size <- 100000
lambda <- 32
pois_vals <- integer()

## FUNCION PARA GENERAR NUESTROS VALORES DE LA DISTRIBUCION POISSON ##
gen_poisson_values <- function(lambda) {
  sum <- 0
  count <- 0
  while (sum < lambda) {
    sum <- sum - log(runif(1))
    count <- count + 1
  }
  return(count - 1)
}

## GENERANDO NUESTOR VECTOR DE VALORES ##
for (i in 1:size) {
  pois_vals <- append(pois_vals, gen_poisson_values(lambda))
}
min_val <- min(pois_vals)
max_val <- max(pois_vals)
min_val
max_val

## EJERCICIO 2 - HISTOGRAMA DE LA DISTRIBUCION DE FRECUENCIAS ##
hist(pois_vals,
      main = paste("Poisson Distribution Simulation",
                    size, "values, lambda =", lambda
                  ),
      xlab = "Generated Values",
      xlim = c(min_val, max_val),
      breaks = max_val - min_val,
      col = "#79cc5b"
    )

## EJERCICIO 3 - ESTIMACION DE PROBABILIDADES DE INTERVALOS ##
calc_ppois_interval <- function(inferior, superior, lambda) {
  return(ppois(superior, lambda) - ppois(inferior, lambda))
}
calc_array_interval <- function(inferior, superior, array_vals, size) {
  return((length(which(pois_vals < superior)) -
          length(which(pois_vals < inferior))) / size)
}
calc_interval <- function(inferior, superior, lambda, array_vals, size) {
  prob_dist <-  calc_ppois_interval(inferior, superior, lambda)
  prob_sample <- calc_array_interval(inferior, superior, array_vals, size)
  error <- abs(prob_dist - prob_sample)
  message(sprintf("Interval: [%f, %f]", inferior, superior))
  message(sprintf("\t- Ppois Function Values -> %f", prob_dist))
  message(sprintf("\t- Generated Values -> %f", prob_sample))
  message(sprintf("\t- Error -> %f", error))
}

## Necesitamos estos tres valores para calcular nuestros intervalos
difference <- (max_val - min_val) / 3
first_third <- min_val + difference
second_third <- min_val + 2 * difference
difference
first_third
second_third
## Tomamos 3 intervalos:
##  [min_val, first_third]
calc_interval(min_val, first_third, lambda, pois_vals, size)
## [first_third, second_third]
calc_interval(first_third, second_third, lambda, pois_vals, size)
## [second_third, max_val]
calc_interval(second_third, max_val, lambda, pois_vals, size)

## EJERCICIO 4 - VALORES PUNTUALES DE LA MEDIA Y LA VARIANZA MUESTRAL ##
pois_mean <- mean(pois_vals)
pois_var <- var(pois_vals)
pois_mean
pois_var

## EJERICICIO 5 - APLICACION DEL TEOREMA CENTRAL DEL LIMITE ##
clt_function <- function(x) {
  num_aux <- (x - pois_mean) / sqrt(pois_var)
  return(num_aux)
}

clt_values <- sapply(pois_vals, clt_function)
clt_mean <- mean(clt_values)
clt_sd <- sd(clt_values)
clt_mean
clt_sd

## OBTENEMOS LOS HISTOGRAMAS DE AMBAS DISTRIBUCIONES DE VALORES ##
clt_hist <- hist(clt_values,
                  main = "CLT Function applied to values",
                  xlab = "Generated Values"
                )

sample_normal <- rnorm(size)
norm <- hist(sample_normal,
              main = "Normal Distribution Values",
              xlab = "Generated Values",
            )

## PONEMOS LAS GRAFICAS SOLAPADAS PARA OBSERVAR LAS DIFERENCIAS :> #
plot(clt_hist,
      main = "Histogram Comparison",
      xlab = "Generated Values",
      col = rgb(0, 0, 1, 0.45)
    )
plot(norm,
      col = rgb(255 / 255, 192 / 255, 203 / 255, 0.45),
      add = T
    )
