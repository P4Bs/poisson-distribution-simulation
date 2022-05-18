## TRABAJITO TO CHINGON DE R - PABLO MORENO Y JUAN MARQUES GARRIDO :>

## ACERCA: HEMOS DECIDIDO HACER NUESTRO TRABAJO CON LA DISTRIBUCION DE POISSON PARA ASI
## OBTENER LA MAYOR NOTA POSIBLE :D


## EJERCICIO 1 - GENERACION DE UNA MUESTRA DE TAMANYO N

# VAMOS A TRABAJAR CON UNA MUESTRA DE TAMANYO N = 150

### PARAMETROS ###
sample_size <- 100000
poisson_parameter <- 20 ## PROVISIONAL !!!!
pois_values <- vector()

## FUNCION PARA GENERAR NUESTROS VALORES DE POISSON ##
gen_value_poisson <- function(pois_lambda) {
  sum <- 0
  count <- 0
  while (sum < pois_lambda) {
    sum <- sum + -1 * log(runif(1))
    count <- count + 1
  }
  return(count)
}

## GENERANDO NUESTOR VECTOR DE VALORES ##
n_iter <- 0;
repeat{
  pois_values <- c(pois_values, gen_value_poisson(poisson_parameter))
  n_iter <- n_iter + 1

  if (n_iter >= sample_size) {
    break
  }
}

## GENERAMOS NUESTRO HISTOGRAMA PARA LA DISTRIBUCION DE FRECUENCIAS ##
hist(pois_values,
     main = paste("Poisson Distribution Simulation", sample_size,
                  "values, lambda =", poisson_parameter),
     xlab = "Generated Values",
     xlim = c(0, poisson_parameter * 2),
     breaks = poisson_parameter * 2,
     col = "#79cc5b"
     )
