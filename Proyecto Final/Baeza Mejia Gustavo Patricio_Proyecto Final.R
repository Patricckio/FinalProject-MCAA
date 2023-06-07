################################################################################
#     Métodos Cuantitativos Aplicados a la Administración - Proyecto Final     #
#   Análisis de las Ventas de Vehículos en México de Enero 2005 a Abril 2023   #
#                      por Baeza Mejia Gustavo Patricio                        #
################################################################################

library(httr)
library(readxl)
library(tidyr)
library(stringr)
library(ggplot2)
library(dplyr)
library(ggthemes)

if (!file.exists("Proyecto Final/VehículosMéxico2005-2023.xlsx")){
  if(!dir.exists("Proyecto Final")){
    dir.create("Proyecto Final")
  }
  url <- "https://www.inegi.org.mx/contenidos/datosprimarios/iavl/tabulados/8_Ventas_serie.xlsx"
  GET(url, write_disk("Proyecto Final/VehículosMéxico2005-2023.xlsx"))
}

Vehiculos <- read_excel("Proyecto Final/VehículosMéxico2005-2023.xlsx", range = "A6:J84242", col_types = c("text","numeric", "text", "text", "text", "text", "text", "text", "text", "numeric"))
Vehiculos

marks_no_sci <- function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)

Ventas_Año <- aggregate(Cantidad ~ Año, Vehiculos, FUN = sum)

ggplot(Ventas_Año, aes(x=Año, y=Cantidad))+
  geom_bar(stat="identity", width=.5, fill="darkblue") +
  geom_text(aes(y= Cantidad, label = scales::comma(Cantidad)),
            position = position_dodge(width = 0.9), size = 3.5, vjust = -1, hjust = 0.5, col = "black") +
  labs(title="Venta de Vehículos en México por Año de 2005 a 2023", x= "Año", y="Unidades",
       caption="Fuente de Datos: INEGI") +
  scale_y_continuous(labels = marks_no_sci) +
  theme_economist()

Ventas_Marca <- aggregate(Cantidad ~ Marca, Vehiculos, FUN = sum)

ggplot(Ventas_Marca, aes(x=Marca, y=Cantidad))+
  geom_bar(stat="identity", width=.5, fill="darkgreen") +
  geom_text(aes(y= Cantidad, label = scales::comma(Cantidad)),
            position = position_dodge(width = 0.9), size = 3, vjust = 0.3, hjust = -0.1, col ="black") +
    labs(title="Venta de Vehículos en México por Marca de 2005 a 2023", x= "Marca", y="Unidades",
       caption="Fuente de Datos: INEGI") +
  scale_y_continuous(labels = marks_no_sci) +
  coord_flip() +
  theme_economist()

Ventas_Segmento <- aggregate(Cantidad ~ Segmento, Vehiculos, FUN = sum)

ggplot(Ventas_Segmento, aes(x=Segmento, y=Cantidad))+
  geom_bar(stat="identity", width=.5, fill="darkred") +
  geom_text(aes(y= Cantidad, label = scales::comma(Cantidad)),
            position = position_dodge(width = 0.9), size = 5, vjust = -2, hjust = 0.3, col = "black") +
  labs(title="Venta de Vehículos en México por Segmento de 2005 a 2023", x= "Segmento", y="Unidades",
       caption="Fuente de Datos: INEGI") +
  scale_y_continuous(labels = marks_no_sci) +
  coord_flip() +
  theme_economist()

Ventas_Origen <- aggregate(Cantidad ~ Origen, Vehiculos, FUN = sum)

ggplot(Ventas_Origen, aes(x=Origen, y=Cantidad))+
  geom_bar(stat="identity", width=.5, fill="darkorange") +
  geom_text(aes(y= Cantidad, label = scales::comma(Cantidad)),
            position = position_dodge(width = 0.9), size = 5, vjust = -0.5, hjust = 0.5, col = "black") +
  labs(title="Origen de los Vehículos Vendidos en México de 2005 a 2023", x= "Origen", y="Unidades",
       caption="Fuente de Datos: INEGI") +
  scale_y_continuous(labels = marks_no_sci) +
  theme_economist()
