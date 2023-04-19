# =========================================================================

# SCRIPT DEVELOPED FOR THE COURSE
# R PROGRAMMING IN HYDROLOGY
# USING THE DOUBLE MASS ANALYSIS METHODOLOGY
# FROM THE BOOK OF MAXIMO VILLON 

# SCIRPT DESARROLLADO PARA EL CURSO
# PROGRAMACION DE R EN HIDROLOGIA
# EMPLEANDO LA NMETODOLOGIA DEL ANALISIS DE DOBLE MASA
# DEL LIBRO DE MAXIMO VILLON

# =========================================================================

# Install and load libraries ----------------------------------------------

install.packages('zoo')
install.packages('tidyverse')
# install.packages('ggarrange')
install.packages('ggpubr')

library(zoo)
library(tidyverse)
# library(ggarrange)
library(ggpubr)

# Load data ---------------------------------------------------------------

Vol_data <- read.csv(file = 'Data.csv', header = T)
head(Vol_data)

# Transforming data -------------------------------------------------------

# transformed to zoo format
zoo_data <- zoo(x = Vol_data[-1], order.by = format(Vol_data[,1], format = '%Y'))

# transformed to data frame format
Vol_data2 <- data.frame(Vol_data) %>% 
  dplyr::mutate(VolAcum = cumsum(Vol_data[,2]))

Vol <- ggplot(data = Vol_data2, mapping = aes(x = Date, y = Vol_mm3)) +
  geom_line(col = 'green4') +  
  labs(title = 'Grafico de Volumenes',
       x = 'Años', y = 'Volumen mm3') + theme_bw()

VolAcum <- ggplot(data = Vol_data2, mapping = aes(x = Date, y = VolAcum)) +
  geom_line(col = 'blue') + 
  labs(title = 'Grafico de Volumenes acumulados',
       x = 'Años', y = 'Volumen acumulado mm3') + theme_bw()

plot_vol <- ggarrange(Vol, VolAcum)

plot_vol

# Save plot

ggsave(filename = 'Volumenes', plot = plot_vol, device = 'png',
       width = 2400, height = 1000, units = "px")

# Calculation of statistics ------------------------------------------------
# Calculo de estadisticos

# Period 1 1964 - 1985
# Period 2 1986 - 1999

# For period 1 1964 - 1985
Pr1 <- window(x = zoo_data, 
              start = 1964, 
              end = 1985)

n1 <- length(Pr1)
mean1 <- mean(Pr1)
sd1 <- sd(Pr1)
sd1_2 <- sd1^2

# For period 2 1986 - 1999
Pr2 <- window(x = zoo_data, 
              start = 1986, 
              end = 1999)

n2 <- length(Pr2)
mean2 <- mean(Pr2)
sd2 <- sd(Pr2)
sd2_2 <- sd2^2

# Evaluation in the consistency in the mean -------------------------------
# Evaluacion de la consistencia en la media

sp <- (((n1-1)*sd1_2 + (n2-1)*sd2_2)/(n1+n2-2))^0.5
sp

sd <- sp*(1/n1 + 1/n2)^0.5
sd

# t.test(zoo_data)

tc <- (mean1 - mean2) / sd

# t.test(Pr1, Pr2)
# qt(0.05 / 2, df = 34)

tt = 1.960 # Table A.5

# Conditional T-student and F-test ----------------------------------------
# Condicional para la prueba T-student y F-test

# if (abs(tc) == tt) {
#   print('NO SE CORRIGE')
# 
# } else {
# 
#   print('SE CORRIGE')
# 
# }

if (abs(tc) == tt) {
  print('NO SE CORRIGE')
  
} else {
  
  print('SE CORRIGE pureba T')
  
  if (sd2_2 > sd1_2) {
    
    print('Se calcula FC')
    
    Fc = sd2_2 / sd1_2
    # var.test(x = Pr2, y = Pr1)
    
  } else {
    
    print('No se caulula FC')
  
  }
  
}

# Correction for period 1 -------------------------------------------------
# Correccion del periodo 1

Corr <- ((Pr1 - mean1) / sd1 ) * sd2 + mean2

New_Vol <- rbind(Corr, Pr2)

class(New_Vol)

write.zoo(x = New_Vol, file = 'New_vol_c.csv', sep = ';')

# Plotting new corrected values -------------------------------------------
# Graficando nuevos valores corregidos

Vol_dataN <- data.frame(Date = as.integer(index(New_Vol)),
                        Vol_mm3 = as.numeric(New_Vol$Vol_mm3)) %>% 
  dplyr::mutate(VolAcum = as.numeric(cumsum(New_Vol)))

Vol2 <- ggplot(data = Vol_dataN, mapping = aes(x = Date, y = Vol_mm3)) +
  geom_line(col = 'green2') +  
  labs(title = 'Grafico de Volumenes Corregidos',
       x = 'Años', y = 'Volumen mm3') + 
  theme_bw()

VolAcum2 <- ggplot(data = Vol_dataN, mapping = aes(x = Date, y = VolAcum)) +
  geom_line(col = 'red4') + 
  labs(title = 'Grafico de Volumenes acumulados Corregidos',
       x = 'Años', y = 'Volumen acumulado mm3') + 
  theme_bw()

plot_vol2 <- ggarrange(Vol2, VolAcum2)
plot_vol2

plot_vol3 <- ggarrange(plot_vol, plot_vol2, nrow = 2)
plot_vol3

# saving graph

ggsave(filename = 'Volumenes_Corregidos', plot = plot_vol3, device = 'png',
       width = 3000, height = 1500, units = "px")

