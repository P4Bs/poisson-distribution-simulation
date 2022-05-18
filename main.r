## TRABAJITO TO CHINGON DE R - PABLO MORENO Y JUAN MARQUï¿½S GARRIDO :>

## ACERCA: HEMOS DECIDIDO HACER NUESTRO TRABAJO CON LA DISTRIBUCION DE POISSON PARA ASI
## OBTENER LA MAYOR NOTA POSIBLE :D


## EJERCICIO NÂº 1 - GENERACION DE UNA MUESTRA DE TAMAÃO N

# VAMOS A TRABAJAR CON UNA MUESTRA DE TAMAÃO N = 150

## FUNCION PARA GENERAR NUESTROS VALORES DE POISSON ##
### PARAMETROS
sample_size <- 20000;
poisson_parameter <- 20 ## PROVISIONAL !!!!
pois_values <- vector();

gen_value_poisson <- function(pois_lambda) {
  sum <- 0;
  count <- 0;
  while (sum < pois_lambda) {
    sum <- sum + -1 * log(runif(1));
    count <- count + 1;
  }
  return(count);
}

n_iter <- 0;
repeat{
  pois_values <- c(pois_values, gen_value_poisson(poisson_parameter));
  n_iter <- n_iter + 1;
  
  if (n_iter >= sample_size) {
    break;
  }
}
hist(pois_values,
     main=paste("Poisson Distribution Simulation with",sample_size, 
                "values and λ=", poisson_parameter),
     xlab="Generated Values",
     xlim=c(0,poisson_parameter*2),
     breaks=poisson_parameter*2,
     col='pink'
     )

## OBTENEMOS NUESTROS NUMEROS ALEATORIOS

## AQUI IRIA UN CODIGO TO WAPO QUE TENGO QUE AVERIGUAR