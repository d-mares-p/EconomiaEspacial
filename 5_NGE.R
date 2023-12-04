#Modelo de la NGE
#Fuente: Thomas de Graaff (4 febrero 2018)
#en: https://thomasdegraaff.nl/post/new-economic-geography-model-with-r/

#librerías
###############################################################

library(nleqslv)  # para resolver el sistema de ecuaciones no lineales
library(ggplot2)  # para gráficos estructurados
library(ggthemes) # tema de economía
library(dplyr)    # para análisis de datos
library(cowplot)  # para combinar gráficos


# Parámetros
################################################################

L       <- 2.0  # Total fuerza de trabajo
phi1    <- 0.48 # fracción de trabajadores que están región 1 y producen alimentos
gam     <- 0.3  # fracción de trabajadores de la industria manufacturera
eps     <- 5.0  # elasticidad de la demanda
rho     <- 0.8  # parámetro de sustitución de la variedad de productos manufacturados
bet     <- 0.8  # costos variables
alp     <- 0.08 # costos fijos
delta   <- 0.4  # presupuesto compartido de fabricación


# Iteraciones del costo de transporte y parámetro lambda
################################################################

iter_l <- 999
step_l <- 0.001
start_l <- 0.001

iter_t <- 51
start_t <- 1.5
step_t <- 0.01


# Función óptima
################################################################

equilibrium <- function(x){
  
  Y1 <- x[1]
  Y2 <- x[2]
  W1 <- x[3]
  W2 <- x[4]
  I1 <- x[5]
  I2 <- x[6]
  
  y <- rep(NA, length(x))
  
  y[1] <- Y1-phi1*(1-gam)*L-lam*gam*L*W1
  y[2] <- Y2-(1-phi1)*(1-gam)*L-(1-lam)*gam*L*W2
  y[3] <- W1-rho*bet^(-rho)*(delta/(alp*(eps-1)))^(1/eps)*(Y1*I1^(eps-1)+T^(1-eps)*Y2*I2^(eps-1))^(1/eps)
  y[4] <- W2-rho*bet^(-rho)*(delta/(alp*(eps-1)))^(1/eps)*(T^(1-eps)*Y1*I1^(eps-1)+Y2*I2^(eps-1))^(1/eps)
  y[5] <- I1-(gam*L/(alp*eps))^(1/(1-eps))*(bet/rho)*(lam*W1^(1-eps)+(1-lam)*T^(1-eps)*W2^(1-eps))^(1/(1-eps))
  y[6] <- I2-(gam*L/(alp*eps))^(1/(1-eps))*(bet/rho)*(lam*T^(1-eps)*W1^(1-eps)+(1-lam)*W2^(1-eps))^(1/(1-eps))
  
  return(y)
}



# Creación del vector donde se almacena la salida
# es el marco de datos de equilibrio para encontrar los equilibrios estables e inestables
################################################################

rel       <- vector(length = iter_l*iter_t)
lambda    <- vector(length = iter_l*iter_t)
transport <- vector(length = iter_l*iter_t)
welfare   <- vector(length = iter_l*iter_t)
w_man_h   <- vector(length = iter_l*iter_t)
w_man_f   <- vector(length = iter_l*iter_t)
w_farm_h  <- vector(length = iter_l*iter_t)
w_farm_f  <- vector(length = iter_l*iter_t)

################################################################
# Configurar el bucle doble para la solución óptima utilizando el paquete nleqslv
# El efecto interior es gama, el efecto exterior es sobre los costos de transporte 
################################################################

# Completamente parametrizado
loop_transport <- seq( start_t, start_t + iter_t * step_t - step_t, by = step_t)
loop_gamma <- seq( start_l, start_l + iter_l * step_l - step_l, by = step_l )
equilibria <- data.frame(T = numeric(0), gamma = numeric(0), stable = numeric(0))

# Valores iniciales
start <- c(1,1,1,1,1,1)

iteration <- 0 # mostrador general
for (T in loop_transport){
  iter_eq <- 0 # contador para encontrar los equilibrios de lambda
  lam_vec <- vector(length = iter_l) # inicializar vector lambda 
  t_vec   <- vector(length = iter_l) # inicializar vector transporte 
  rel_vec <- vector(length = iter_l) # inicializar vector con salarios reales relativos
  for (lam in loop_gamma){
    iteration <- iteration + 1
    iter_eq   <-  iter_eq + 1
    opt <- nleqslv(start, equilibrium)
    Y1 <- opt$x[1]
    Y2 <- opt$x[2]
    W1 <- opt$x[3]
    W2 <- opt$x[4]
    I1 <- opt$x[5]
    I2 <- opt$x[6]
    
    # Para el resto de los vectores
    rel[iteration]       <- (W1/I1^delta)/(W2/I2^delta)
    welfare[iteration]   <- Y1/(I1^delta)+Y2/(I2^delta)
    w_man_h[iteration]   <- W1/I1^delta
    w_man_f[iteration]   <- W2/I2^delta
    w_farm_h[iteration]  <- 1/I1^delta
    w_farm_f[iteration]  <- 1/I2^delta
    lambda[iteration]    <- lam
    transport[iteration] <- T
    
    # Necesario para encontrar los equilibrios (un poco redundante pero más legible)
    lam_vec[iter_eq]     <- lam
    t_vec[iter_eq]       <- T
    rel_vec[iter_eq]     <- (W1/I1^delta)/(W2/I2^delta)
  }
  eq <- data.frame(t_vec, lam_vec, rel_vec)
  eq <- eq %>%
    mutate(
      dpos = ifelse( ( (rel_vec - 1) >= 0 & ( lag(rel_vec) - 1)  < 0), 1, 0 ),
      dneg = ifelse( ( (rel_vec - 1) <= 0 & ( lag(rel_vec) - 1)  > 0), 1, 0 )
    )
  stable <- eq %>%
    filter(dneg == 1) %>%
    mutate(stable =1) %>%
    select(-dpos)
  unstable <- eq %>%
    filter(dpos == 1) %>%
    mutate(stable =0) %>%
    select(-dneg)
  if (nrow(stable) > 0 ) {
    equilibria <- rbind(equilibria, data.frame(stable[1], stable[2], stable[5]))
  }
  if (nrow(unstable) > 0) {
    equilibria <- rbind(equilibria, data.frame(unstable[1], unstable[2], unstable[5]))
  }
  if (nrow(unstable) == 1){
    equilibria <- rbind(equilibria, c(unstable[1,1], 0, 1))
    equilibria <- rbind(equilibria, c(unstable[1,1], 1, 1))
  }
  if ( (nrow(unstable) == 1) & (nrow(stable) == 1) ) {
    if (stable$lam_vec[1] > unstable$lam_vec[1] ){
      equilibria <- rbind(equilibria, c(unstable[1,1], 0, 1))
    }
    if (stable$lam_vec[1] < unstable$lam_vec[1] ){
      equilibria <- rbind(equilibria, c(unstable[1,1], 1, 1))
    }
  }
  if ((nrow(unstable) + nrow(stable)) == 3) {
    equilibria <- rbind(equilibria, c(unstable[1,1], 1, 1))
    equilibria <- rbind(equilibria, c(unstable[1,1], 0, 1))
  }
}

################################################################
# Crear el dataframe llamado neg_data
################################################################

neg_data <- data.frame(transport, lambda, rel, welfare,
                       w_man_h, w_man_f, w_farm_h, w_farm_f)

################################################################
# Crear gráficas
################################################################

#Indicar qué líneas deben resaltarse
top_line <- neg_data[neg_data$transport == "1.5", ]
bottom_line <- neg_data[neg_data$transport == "2", ]
mid_line <- neg_data[neg_data$transport == "1.75", ]


ggplot(neg_data) + aes(lambda, rel, group = transport) + geom_line(size = 0.5, colour="grey", alpha = 0.5) +
  geom_line(data = top_line, aes(x = lambda, y = rel, group = transport, colour = "steelblue"), size = 1) +
  geom_line(data = bottom_line, aes(x = lambda, y = rel, group = transport, colour = "black"), size = 1) +
  geom_line(data = mid_line, aes(x = lambda, y = rel, group = transport, colour = "red"), size = 1) +
  scale_colour_discrete(name = "Costos de transporte", labels = c("Alto", "Medo", "Bajo")) +
  geom_hline(yintercept = 1, size = 1, colour = "red", linetype = 4) +
  theme_economist() +
  labs(title ="Diagrama tipo Wiggle", y = "Salario real relativo",
       subtitle = "Cambios en salarios reales relativos con variables lambda y costos de transporte")


ggplot(equilibria) + aes(t_vec, lam_vec) +
  geom_point(aes(colour = factor(stable))) +
  theme_economist() +
  theme(legend.title=element_blank()) +
  scale_colour_discrete(breaks = c("0", "1"), labels=c("Equilibrio inestable", "Equilibrio estable")) +
  labs(title ="Diagrama tipo Tomahawk", y = "lambda", x = "costos de transporte")



