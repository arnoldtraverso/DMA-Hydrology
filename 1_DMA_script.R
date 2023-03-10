

# Install and load libraries ----------------------------------------------

install.packages('zoo')

library(zoo)
library(tidyverse)
library(ggarrange)
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

# Save plot

ggsave(filename = 'Volumenes', plot = plot_vol, device = 'png',
       width = 2400, height = 1000, units = "px")

# Estadisticos





