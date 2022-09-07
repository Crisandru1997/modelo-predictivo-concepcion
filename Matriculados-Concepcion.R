# Paquetes requeridos
#install.packages("ggmap")
#install.packages("rio")
#install.packages("utils")
#install.packages("tidyverse")
#install.packages("leaflet")
#install.packages("dplyr")
#install.packages("maps")
#install.packages("htmltools")

# Cargamos los paquetes
library(ggmap)
library(rio)
library(utils)
library(tidyverse)
library(leaflet)
library(dplyr)
library(maps)
library(htmltools)

# Obtenemos la Ubicación de Concepción, gracias a la latitud y longitud
mapa <- leaflet() %>% setView(lng = -73.0497700, lat = -36.8269900, zoom = 14)

# Dibujamos el mapa y lo guardamos
concepcion <- mapa %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron)

# Traemos las universidades
# Seleccionar el archivo MatriculadosConcepcion.csv dentro de la carpeta Archivos.
matriculados_totales <- file.choose()
matriculados_totales <- read_csv(matriculados_totales)
matriculados_totales
# Separamos los datos según la cantidad de matriculados en cada año
matriculados_2022 <- matriculados_totales[1:18, ]
matriculados_2021 <- matriculados_totales[19:36, ]
matriculados_2020 <- matriculados_totales[37:55, ]
matriculados_2019 <- matriculados_totales[56:73, ]

# Ordenamos el data según la cantidad de matriculados totales por universidad
matriculados_2019 <- matriculados_2019[order(matriculados_2019$matriculatotal, decreasing = TRUE), ]
matriculados_2020 <- matriculados_2020[order(matriculados_2020$matriculatotal, decreasing = TRUE), ]
matriculados_2021 <- matriculados_2021[order(matriculados_2021$matriculatotal, decreasing = TRUE), ]
matriculados_2022 <- matriculados_2022[order(matriculados_2022$matriculatotal, decreasing = TRUE), ]

# Creamos el mapa detallando la cantidad de matriculados según el año

# Estilos del titulo principal del mapa en CSS.
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    border-radius: 15px;
    font-weight: bold;
    font-size: 26px;
  }
"))

# ============================= Matriculados totales el año 2019 en Concepción =============================
grafico_matriculados_2019 <- concepcion %>% addCircles(data = matriculados_2019, lat = ~latitud, lng = ~longitud, radius = ~sqrt(matriculatotal)*9, popup = paste("<b>",matriculados_2019$universidad,"</b></br>",
                                                                                                                                                                  "<b>Total matriculados: </b>",matriculados_2019$matriculatotal,"</br>",
                                                                                                                                                                  "<b>Año: </b>", matriculados_2019$age
), color = "#E85D04", stroke = FALSE, fillOpacity = 0.4, dashArray = NULL, label = ~htmlEscape(universidad),  group = "Matricula Total") %>%
  addTiles() %>%
  addControl(tags$div(tag.map.title, HTML("Matriculados 2019")) , position = "topleft", className="map-title")

# Visualización
grafico_matriculados_2019

# ============================= Matriculados totales el año 2020 en Concepción =============================
grafico_matriculados_2020 <- concepcion %>% addCircles(data = matriculados_2020, lat = ~latitud, lng = ~longitud, radius = ~sqrt(matriculatotal)*9, popup = paste("<b>",matriculados_2020$universidad,"</b></br>",
                                                                                                                                                                  "<b>Total matriculados: </b>",matriculados_2020$matriculatotal,"</br>",
                                                                                                                                                                  "<b>Año: </b>", matriculados_2020$age
), color = "#E85D04", stroke = FALSE, fillOpacity = 0.4, dashArray = NULL, label = ~htmlEscape(universidad),  group = "Matricula Total") %>%
  addTiles() %>%
  addControl(tags$div(tag.map.title, HTML("Matriculados 2020")) , position = "topleft", className="map-title")

# Visualización
grafico_matriculados_2020

# ============================= Matriculados totales el año 2021 en Concepción =============================
grafico_matriculados_2021 <- concepcion %>% addCircles(data = matriculados_2021, lat = ~latitud, lng = ~longitud, radius = ~sqrt(matriculatotal)*9, popup = paste("<b>",matriculados_2021$universidad,"</b></br>",
                                                                                                                                                                  "<b>Total matriculados: </b>",matriculados_2021$matriculatotal,"</br>",
                                                                                                                                                                  "<b>Año: </b>", matriculados_2021$age
), color = "#E85D04", stroke = FALSE, fillOpacity = 0.4, dashArray = NULL, label = ~htmlEscape(universidad),  group = "Matricula Total") %>%
  addTiles() %>%
  addControl(tags$div(tag.map.title, HTML("Matriculados 2021")) , position = "topleft", className="map-title")

# Visualización
grafico_matriculados_2021

# ============================= Matriculados totales el año 2022 en Concepción =============================
grafico_matriculados_2022 <- concepcion %>% addCircles(data = matriculados_2022, lat = ~latitud, lng = ~longitud, radius = ~sqrt(matriculatotal)*9, popup = paste("<b>",matriculados_2022$universidad,"</b></br>",
                                                                                                                                                                  "<b>Total matriculados: </b>",matriculados_2022$matriculatotal,"</br>",
                                                                                                                                                                  "<b>Año: </b>", matriculados_2022$age
), color = "#E85D04", stroke = FALSE, fillOpacity = 0.4, dashArray = NULL, label = ~htmlEscape(universidad),  group = "Matricula Total") %>%
  addTiles() %>%
  addControl(tags$div(tag.map.title, HTML("Matriculados 2022")) , position = "topleft", className="map-title")

# Visualización
grafico_matriculados_2022

# ============================= Creación del modelo predictivo para estudiantes matriculados el año 2023 =============================

# Creamos la regresión lineal simple con la funcion lm (linear model)
regresion <- lm(matriculatotal ~ age + universidad, data=matriculados_totales)

# visualización opcional
# summary(regresion)

# Cargaremos el csv que contiene las latitudes y longitudes de las universidades, para agregar los matriculados obtenidos con el modelo predictivo.
# Seleccionar el archivo 2023CONCE.csv dentro de la carpeta Archivos.
matriculados_2023 <- file.choose()
matriculados_2023 <- read_csv(matriculados_2023)

# Generamos el modelo de predicción.
prediccion <- predict(regresion, matriculados_2023)
prediccion
#Guardamos el data completo con la información obtenida en el modelo predictivo.
matriculados_2023MP <- data.frame(age=2023, universidad=matriculados_2023$universidad, matriculatotal=prediccion, latitud=matriculados_2023$latitud, longitud=matriculados_2023$longitud)
matriculados_2023MP
# Ordenamos el data según la cantidad de metriculados para insertarlo en el mapa.
matriculados_2023MP <- matriculados_2023MP[order(matriculados_2023MP$matriculatotal, decreasing = TRUE), ]

# El radius que se encuentra dentro de la función de addCircles, se encuentra en metros.
grafico_matriculados_2023 <- concepcion %>% addCircles(data = matriculados_2023MP, lat = ~latitud, lng = ~longitud, radius = ~sqrt(matriculatotal)*9, popup = paste("<b>",matriculados_2023MP$universidad,"</b></br>",
                                                                                                                                                                    "<b>Total matriculados: </b>",as.integer(matriculados_2023MP$matriculatotal),"</br>",
                                                                                                                                                                    "<b>Año: </b>", matriculados_2023MP$age
), color = "#E85D04", stroke = FALSE, fillOpacity = 0.4, dashArray = NULL, label = ~htmlEscape(universidad),  group = "Matricula Total") %>%
  addTiles() %>%
  addControl(tags$div(tag.map.title, HTML("Matriculados 2023")) , position = "topleft", className="map-title")

# Visualización
grafico_matriculados_2023

