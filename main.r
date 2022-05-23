## Outputs and Strings ##
interval_format <-
          "Sample Probability: %f | Poisson Probability: %f | Error: %f"

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
  return(count)
}

## GENERANDO NUESTOR VECTOR DE VALORES ##
for (i in 1:size) {
  pois_vals <- append(pois_vals, gen_poisson_values(lambda))
}
min_val <- min(pois_vals)
max_val <- max(pois_vals)

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
error_sum <- 0
for (i in min_val:max_val) {
  prob_dist <- ppois(i, lambda)
  prob_sample <- length(which(pois_vals < i)) / size

  error <- abs(prob_dist - prob_sample) / prob_dist
  error_sum <- error_sum + error
  print(sprintf(interval_format,
                prob_sample,
                prob_dist,
                error
                )
        )
}
sprintf("Mean error : %f", error_sum / size)

## EJERCICIO 4 - VALORES PUNTUALES DE LA MEDIA Y LA VARIANZA MUESTRAL ##
pois_mean <- mean(pois_vals)
pois_var <- var(pois_vals)

## EJERICICIO 5 - APLICACION DEL TEOREMA CENTRAL DEL LIMITE ##
clt_function <- function(x) {
  num_aux <- (x - pois_mean) / sqrt(pois_var)
  return(num_aux)
}

clt_values <- sapply(pois_vals, clt_function)
clt_mean <- mean(clt_values)
clt_sd <- sd(clt_values)

# MEDIA Y VARIANZA DE LA DISTRIBUCION TRAS APLICAR CLT FUNCTION #
clt_mean
clt_var

## OBTENEMOS LOS HISTOGRAMAS DE AMBAS DISTRIBUCIONES DE VALORES ##
clt_hist <- hist(clt_values,
                  main = "CLT Function applied to values",
                  xlab = "Generated Values"
                )

sample_normal <- rnorm(100000)
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