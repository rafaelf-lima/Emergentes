library(tidyverse)
library(ggrepel)
library(bbplot)
pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')
#COLUNAS: select() e mutate()
#LINHAS: filter(), distinct() e arrange()
#AGRUPAR: group_by(), summarise() e count()

dados_emerg <- read.csv(file = "C:\\Users\\ritad\\Downloads\\Life expectancy.csv")
view(dados_emerg)

paises <- c("Brazil", "Mexico", "India", "China", "Russia")
anos <- c("1950", "2016")

dados_pemerg <- dados_emerg %>% 
  filter(Entity %in% paises) %>% 
  filter(Year == 1950 | Year == 2016) %>%
  mutate(Expectativa = round(Life.expectancy)) %>% 
  rename(Anos = Year) %>% 
  rename(Países = Entity) %>% 
  select(-Life.expectancy) %>% 
  spread(Anos, Expectativa) %>%
  mutate(Diferença = `2016` - `1950`) 

view(dados_pemerg)  
  
ggplot(dados_pemerg, aes(x = `1950`, xend = `2016`, y= reorder(Países, Diferença), group = Países))+
  geom_dumbbell(colour = "#d3a625",
                size = 2.5,
                colour_x = "#314263",
                size_x = 4,
                colour_xend = "#ae0001",
                size_xend = 4)+
  scale_y_discrete(labels = c("Brazil" = "Brasil", "China" = "China", "India" = "Índia", "Mexico" = "México", "Russia" = "Rússia"))+
  theme(
    axis.ticks.x = element_line(colour = "#333333"), 
    axis.ticks.length =  unit(0.6, "cm"))+
  bbc_style()+
  labs(title="O aumento na expectativa de vida de países emergentes\nentre os anos de 1950 e 2016",
       subtitle="Ao longo dos 66 anos, todos os países no gráfico registraram significativo aumento na expectativa de vida")
