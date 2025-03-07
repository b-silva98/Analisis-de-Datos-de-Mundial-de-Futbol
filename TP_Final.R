# 1. Mostrar y Cambiar el Directorio de Trabajo
getwd()  # Mostrar el directorio de trabajo actual

# Cambiar el directorio de trabajo al lugar donde están los archivos (ajusta según tu sistema)
setwd("/home/brian/Documentos/BIG DATA/FINAL_MUNDIAL")

# 2. Cargar Librería y Leer Datos desde Excel
library(readxl)  
library(ggplot2)

# Leer los datos desde un archivo Excel específico y una hoja en particular
# Verifica que el archivo y la hoja existan en la ruta especificada
datos_mundial <- read_excel("BDMUNDIAL.xlsx", sheet = "TEA_SeleccionesXmundial")

# 3. Vista Previa de los Datos Cargados
head(datos_mundial) 
View(datos_mundial)

# 4. Cambiar Nombres de las Columnas
colnames(datos_mundial)[1] <- "Anio_Mundial"         
colnames(datos_mundial)[3] <- "Codigo_Seleccion"      
colnames(datos_mundial)[5] <- "Posicion_Seleccion"    
colnames(datos_mundial)[13] <- "Goles_Contra"         
colnames(datos_mundial)[17] <- "Codigo_Entrenador"    
colnames(datos_mundial)[18] <- "Nombre_Entrenador"    

# 5. Crear Subconjuntos de Datos
# Subconjunto con Información de Selección y Desempeño
seleccion_info <- datos_mundial[, c("Anio_Mundial", "Nom_Seleccion", "Posicion_Seleccion", 
                                    "PTS_Seleccion", "PJ_Seleccion", "PG_Seleccion", 
                                    "PE_Seleccion", "PP_Seleccion", "GF_Seleccion", "Goles_Contra")]

print(seleccion_info)
# Subconjunto con Información de Entrenador y Continente
entrenador_continente <- datos_mundial[, c("Anio_Mundial", "Codigo_Seleccion", "Nom_Seleccion", 
                                           "Cod_Continente", "Nom_Continente", 
                                           "Codigo_Entrenador", "Nombre_Entrenador")]
View(entrenador_continente)

# 6. Análisis de Datos
# Dimensiones del dataframe 
dim(datos_mundial)

# Calcular la media de la variable "putos de seleccion"
media_puntos <- mean(datos_mundial$PTS_Seleccion)
print(media_puntos)

# Resumen de las columnas
summary(datos_mundial)

# 7. Verificacion de los objetos
# Verifica la clase del objeto
class(datos_mundial)

# Verifica la clase de la columna 'Anio_Mundial' en el dataframe 'datos_mundial'
class(datos_mundial$Anio_Mundial)

# Verifica la clase de la columna 'Sede_Mundial' (superficie total) en 'datos_mundial'
class(datos_mundial$Sede_Mundial)

# Verifica la clase de la columna 'Codigo_Entrenador'  en 'datos_mundial'
class(datos_mundial$Codigo_Entrenador)

# 8. Graficos
# Gráfico de dispersión entre puntos y goles a favor
plot(datos_mundial$PTS_Seleccion, datos_mundial$GF_Seleccion, 
     xlab = "Puntos de Selección", ylab = "Goles a Favor", 
     main = "Gráfico de Dispersión: Puntos vs Goles a Favor", 
     col = "blue", pch = 19)

# Contar cuántas selecciones hay por continente
conteo_continentes <- table(datos_mundial$Nom_Continente)

# Gráfico de barras de la cantidad de selecciones por continente
barplot(conteo_continentes, 
        col = "green", 
        xlab = "Continentes", ylab = "Cantidad de Selecciones", 
        main = "Cantidad de Selecciones por Continente", 
        las = 1) 

# Gráfico de caja para los puntos de las selecciones
boxplot(datos_mundial$PTS_Seleccion, 
        main = "Distribución de Puntos de Selección", 
        ylab = "Puntos de Selección", 
        col = "orange")

# Histograma de los goles a favor de las selecciones
hist(datos_mundial$GF_Seleccion, 
     main = "Histograma de Goles a Favor", 
     xlab = "Goles a Favor", 
     col = "purple", 
     breaks = max(datos_mundial$GF_Seleccion) - min(datos_mundial$GF_Seleccion) + 1, 
     xaxt = "n")  # desactivar el eje X predeterminado

axis(1, at = seq(0, max(datos_mundial$GF_Seleccion), by = 1))

# Crear un dataframe con las posiciones por continente
posiciones_continente <- table(datos_mundial$Nom_Continente, datos_mundial$Posicion_Seleccion)

# Gráfico de barras apiladas
barplot(posiciones_continente, 
        beside = FALSE, col = c("blue", "red", "green", "yellow", "brown", "orange"), 
        legend = rownames(posiciones_continente), 
        main = "Posiciones por Continente", 
        xlab = "Posición", ylab = "Cantidad de Selecciones")


# Filtramos los datos para Argentina
argentina <- datos_mundial[datos_mundial$Nom_Seleccion == "Argentina", ]
francia <- datos_mundial[datos_mundial$Nom_Seleccion == "Francia", ]

# Combinar los datos en un nuevo dataframe
comparacion <- rbind(argentina, francia)



datos_mundial_2022 <- datos_mundial[datos_mundial$Anio_Mundial == 2022, ]
# Crear el gráfico
ggplot(datos_mundial_2022, aes(x = reorder(Nom_Seleccion, -Goles_Contra), y = Goles_Contra)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Nom_Continente, scales = "free_x") +
  labs(x = "Selección", y = "Goles en Contra", title = "Goles en Contra por Selección y Continente en 2022") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(argentina, aes(x = factor(Anio_Mundial), y = GF_Seleccion)) +
  geom_col(fill = "#0018A8") +
  labs(x = "Año del Mundial", y = "Goles a Favor", 
       title = "Evolución de los goles de Argentina en los mundiales") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


# Filtrar los datos del Mundial 2006
datos_mundial_2006 <- datos_mundial[datos_mundial$Anio_Mundial == 2006, ]

# Seleccionar la selección con el puntaje más alto de cada continente
selecciones_top <- datos_mundial_2006 %>%
  group_by(Nom_Continente) %>%
  filter(PTS_Seleccion == max(PTS_Seleccion)) %>%
  ungroup()

# Crear un gráfico de sectores
ggplot(selecciones_top, aes(x = "", y = PTS_Seleccion, fill = Nom_Seleccion)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Selecciones con Mayor Puntaje por Continente - Mundial 2006",
       fill = "Selección") +
  theme_void()























