#Librerias
library(DT)
library(readxl)
library(ggplot2)
library(readr)
library(dplyr)
library(scales)
library(lubridate)

#Cargar el archivo y mostrar datos
datos <- read_excel("C:/Users/lesly/python_bootcamp/Actividad2_Modulo01/Enhanced_pizza.xlsx")
datatable(datos)

#Estructura del dataset
str(datos)
#Resumen estadistico (General)
summary(datos)

# Eliminar duplicados
datos <- distinct(datos)

#Omitir registros con datos incompletos
datos <- na.omit(datos)

#Solo dos decimales
datos <- datos %>%
  mutate(
    `Delivery Efficiency (min/km)` = round(`Delivery Efficiency (min/km)`, 2),
    `Topping Density` = round(`Topping Density`, 2),
    `Restaurant Avg Time` = round(`Restaurant Avg Time`, 2),
    `Estimated Duration (min)` = round(`Estimated Duration (min)`, 2)
  )
datatable(datos)

#Agrupacion de datos por medio de Restaurant y Location
datos_resumen <- datos %>%
  group_by(`Restaurant Name`, Location) %>%
  summarise(
    total_orders = n(),
    avg_delivery_duration = mean(`Delivery Duration (min)`, na.rm = TRUE),
    avg_delivery_efficiency = mean(`Delivery Efficiency (min/km)`, na.rm = TRUE),
    avg_topping_density = mean(`Topping Density`, na.rm = TRUE),
    avg_restaurant_time = mean(`Restaurant Avg Time`, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    #Para que solo tengan dos decimales
    across(where(is.numeric), ~round(., 2)) 
  )
datatable(datos_resumen)

#Grafico por metodo de pago
ggplot(datos, aes(x = `Payment Method`)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Cantidad de Órdenes por Método de Pago",
    x = "Método de Pago",
    y = "Número de Órdenes"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Grafico Delivery y Tamanio de Pizza
pizza_stats <- datos %>%
  group_by(`Pizza Size`, `Pizza Type`) %>%
  summarise(
    avg_duration = mean(`Delivery Duration (min)`, na.rm = TRUE),
    order_count = n()
  )

ggplot(pizza_stats, aes(x = `Pizza Size`, y = avg_duration, fill = `Pizza Type`)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Duración Promedio de Entrega por Tamaño y Tipo de Pizza",
    x = "Tamaño de Pizza",
    y = "Duración Promedio (min)",
    fill = "Tipo de Pizza"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

#Eficiencia del Restaurante
ggplot(datos, aes(x = `Restaurant Name`, y = `Delivery Efficiency (min/km)`)) +
  geom_boxplot() +
  theme_minimal() +
  coord_flip() +
  labs(title = "Eficiencia de entrega por restaurante")