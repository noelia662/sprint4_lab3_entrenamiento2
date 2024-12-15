# 1. Cargar las librerías y los datos
# Instala y carga las librerías necesarias
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(tidyr)) install.packages("tidyr", dependencies = TRUE)

library(dplyr)
library(tidyr)

# Cargar el dataset mtcars
data(mtcars)
df <- as.data.frame(mtcars)

# 2. Selección de columnas y filtrado de filas
df_seleccionado <- df %>%
  select(mpg, cyl, hp, gear) %>%
  filter(cyl > 4) # Filtrar filas donde cyl > 4

# 3. Ordenación y renombrado de columnas
df_ordenado <- df_seleccionado %>%
  arrange(desc(hp)) %>%
  rename(consumo = mpg, potencia = hp) # Renombrar mpg a consumo y hp a potencia

# 4. Creación de nuevas columnas
# Crear la columna eficiencia antes del left_join
df_ordenado <- df_ordenado %>%
  mutate(eficiencia = consumo / potencia)

# Verificar los resultados de la nueva columna
print("Dataframe después de crear la columna eficiencia:")
print(df_ordenado)

# 5. Creación del segundo dataframe y unión de dataframes
df_transmision <- data.frame(
  gear = c(3, 4, 5),
  tipo_transmision = c("Manual", "Automática", "Semiautomática")
)

# Realizar un left_join para combinar los dataframes por la columna gear
df_unido <- left_join(df_ordenado, df_transmision, by = "gear")

# Verificar los resultados de la unión de dataframes
print("Dataframe después de la unión con el dataframe de transmisión:")
print(df_unido)

# 6. Transformación de formatos
# Primero, realizar la agrupación por cilindros y calcular el consumo medio y la potencia máxima por cilindro
df_agrupado <- df_unido %>%
  group_by(cyl) %>%
  summarise(consumo_medio = mean(consumo, na.rm = TRUE),
            potencia_maxima = max(potencia, na.rm = TRUE))

# Verificar los resultados después de la agrupación
print("Dataframe con el consumo medio y la potencia máxima por cilindro:")
print(df_agrupado)

# Transformación a formato largo
df_largo <- df_unido %>%
  pivot_longer(cols = c(consumo, potencia, eficiencia), 
               names_to = "medida", 
               values_to = "valor")

# Verificar la transformación a formato largo
print("Dataframe en formato largo:")
print(df_largo)

# Identificar y manejar duplicados antes de la transformación a formato ancho
df_largo_agrupado <- df_largo %>%
  group_by(cyl, gear, tipo_transmision, medida) %>%
  summarise(valor_promedio = mean(valor, na.rm = TRUE))

# Verificar la agrupación y el manejo de duplicados
print("Dataframe agrupado y manejando duplicados:")
print(df_largo_agrupado)

# Transformación de nuevo a formato ancho, aplicando una función de agregación
df_ancho <- df_largo_agrupado %>%
  pivot_wider(names_from = medida, values_from = valor_promedio)

# Verificar los resultados después de la transformación a formato ancho
print("Dataframe en formato ancho después de la agregación:")
print(df_ancho)

# 7. Verificación final
# Aquí imprimimos los resultados finales para asegurarnos de que todo el proceso se haya completado correctamente
print("Dataframe final después de todas las operaciones:")
print(df_ancho)
