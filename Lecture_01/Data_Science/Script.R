#Definir librerías

library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)

#Leer Excel

Master <- read_excel("Raw_Data.xlsx")

#Definir variable y agrupar datos por valores de columna

ProdUnits <- Master %>% group_by(Plant) %>% summarize(count = n())

#Creación de Histograma básico

ProdUnitsGraph <- ggplot(data=ProdUnits, aes(x=Plant, y=count)) +
  geom_bar(stat="identity")

ProdUnitsGraph

#Estilización de gráfica

ProdUnitsGraph2 <- ggplot(data=ProdUnits, aes(x=Plant, y=count)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

ProdUnitsGraph2

#Filtrar datos

Planta1 <- Master %>% filter(Plant == "PU1")

#Gráfica avanzada
Planta1$`Work Center` <- substr(Planta1$`Work Center`, 1, 6) 
WC1 = Planta1 %>% group_by(`Work Center`) %>% summarize(count = n())
WC1 = setNames(WC1,c('WC','Quantity')) %>% mutate( WC = reorder(WC,Quantity))

WC1_Graph <- plot_ly(WC1, x = ~WC, y = ~Quantity, type = 'bar', text = text,
        marker = list(color = 'rgb(158,202,225)',
                      line = list(color = 'rgb(8,48,107)',
                                  width = 1.5))) %>% layout(title = "Work Centers")

WC1_Graph

#Limpieza de datos


WC1_GraphT10 <- WC1 %>% top_n(n=10, Quantity) %>% plot_ly(x = ~WC, y = ~Quantity, type = 'bar', text = text,
                     marker = list(color = 'rgb(158,202,225)',
                                   line = list(color = 'rgb(8,48,107)',
                                               width = 1.5))) %>% layout(title = "Work Centers")

WC1_GraphT10


#¿Qué parte es la que más incidencias tiene en la PU #1?

Part1 = Planta1 %>% group_by(`Part Number Affected`) %>% summarize(count = n()) %>% 
  setNames(c('Part','Quantity')) %>% top_n(n=10, Quantity) %>% mutate(Part = reorder(Part,Quantity))

Part1_Graph <- Part1 %>% plot_ly(x = ~Part, y = ~Quantity, type = 'bar', text = text,
                                                          marker = list(color = 'rgb(158,202,225)',
                                                                        line = list(color = 'rgb(8,48,107)',
                                                                                    width = 1.5))) %>% layout(title = "Most Affected Parts")

Part1_Graph


#¿Cuál ha sido el comportamiento histórico de esta parte?

TimeSeries_MostAP = Planta1 %>% filter(`Part Number Affected` == "C01570000-001" ) %>%
  group_by(`Creation Date`) %>% summarize(count = n()) %>% setNames(c('Date','Quantity'))


TimeSeries_MostAPGraph <- plot_ly(TimeSeries_MostAP, type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~Date, y = ~Quantity, name = 'Part') %>%
  layout(showlegend = F)
options(warn = -1)

TimeSeries_MostAPGraph <- TimeSeries_MostAPGraph %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)


TimeSeries_MostAPGraph



#--------------Ejercicios--------------

# 1. ¿Cuál es la parte más afectada de la PU #2?


#2. ¿Cuál es la descripción de esa pieza?


#3. ¿Cuál ha sido su comportamiento histórico?


