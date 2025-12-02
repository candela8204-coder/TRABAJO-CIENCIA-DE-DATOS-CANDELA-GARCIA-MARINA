
# Cargar librerias
library(tidyverse)
library(naniar)
library(corrplot)
library(gridExtra)

# Cargar y revisar estructuras
head(data)
str(data)
summary(data)

head(df3)
str(df3)
summary(df3)

# Valores faltantes
cat("NA en data:\n")
colSums(is.na(data))
gg_miss_var(data, show_pct = TRUE)

cat("NA en df3:\n")
colSums(is.na(df3))
gg_miss_var(df3, show_pct = TRUE)

# Imputamos por media
df3$PreCS[is.na(df3$PreCS)]<- mean(df3$PreCS, na.rm = TRUE)
df3$CS1[is.na(df3$CS1)]<- mean(df3$CS1, na.rm = TRUE)
df3$CS2[is.na(df3$CS2)]<- mean(df3$CS2, na.rm = TRUE)
df3$CS3[is.na(df3$CS3)]<- mean(df3$CS3, na.rm = TRUE)
df3$CS4[is.na(df3$CS4)]<- mean(df3$CS4, na.rm = TRUE)
df3$CS5[is.na(df3$CS5)]<- mean(df3$CS5, na.rm = TRUE)
df3$NA..1[is.na(df3$NA..1)]<- mean(df3$NA..1, na.rm = TRUE)

# Eliminamos NA en data
data <- data %>% filter(Sex != "Avrg")

# Comprobamos 
cat("NA en data:\n")
colSums(is.na(data))
gg_miss_var(data, show_pct = TRUE)


cat("NA en df3:\n")
colSums(is.na(df3))
gg_miss_var(df3, show_pct = TRUE)
 

# Exploración básica
cat("Dimensiones del dataset:\n")
dim(data) 

cat("Resumen de las variables:\n")
summary(data)

cat("Dimensiones del dataset:\n")
dim(df3) 

cat("Resumen de las variables:\n")
summary(df3)

# Estadística descriptiva para variables numéricas. Vamos a utilizar la palabra selec del paquete tidiverse, sobre nuestros datos, va a seleccionar variables numericas
numericas_data <- select(data, where(is.numeric))
cat("Estadísticas descriptivas para variables numéricas:\n")
summary(numericas_data)


numericas_df3 <- select(df3, where(is.numeric))
cat("Estadísticas descriptivas para variables numéricas:\n")
summary(numericas_df3)

# GRÁFICOS
# Crear ID para vincular ambos datasets
# Crear ID para ambos datasets
data$ID <- 1:nrow(data)
df3$ID  <- 1:nrow(df3)

# Crear Freezing medio
df3$Freezing_CS1_5 <- rowMeans(df3[, c("CS1","CS2","CS3","CS4","CS5")], na.rm=TRUE)

# Merge
df_merge <- inner_join(data, df3, by = "ID")


# 1. Heatmap
library(reshape2)
library(ggplot2)

# Seleccionar freezing y regiones relevantes del circuito del miedo
regiones_miedo <- df_merge %>%
  select(
    Freezing_CS1_5,
    CeC, CeL, CeM,
    DMPAG, DLPAG, LPAG, VLPAG,
    BLA, BMA
  )

# Matriz de correlación
corr_matrix <- cor(regiones_miedo, use = "pairwise.complete.obs")

# Convertir a formato largo para ggplot
corr_melt <- melt(corr_matrix)

# Heatmap
ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limits = c(-1, 1),
    name = "Correlación"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14)
  ) +
  labs(
    title = "Correlaciones Freezing-cFos en Regiones del Miedo",
    x = "",
    y = ""
  )

# Barplot actividad CeM
library(tidyverse)

# Seleccionar columnas de freezing y CeM
freezing_cols <- c("PreCS", "CS1", "CS2", "CS3", "CS4", "CS5", 
                   "CS1.5", "CS6.10", "CS11.15", "PreCS.1")
# Calcular correlaciones de cada fase con CeM
correlaciones <- sapply(freezing_cols, function(col){
  cor(df_merge[[col]], df_merge$CeM, use = "complete.obs")
})

# Convertir a dataframe largo para ggplot
df_cor <- data.frame(
  Fase = freezing_cols,
  Correlacion = correlaciones
)
# Graficar
ggplot(df_cor, aes(x = Fase, y = Correlacion, fill = Correlacion)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Correlaciones entre freezing y actividad neuronal en CeM",
    x = "Fase del Freezing",
    y = "Correlación",
    fill = "Correlacion"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Boxplot freezing por sexo y genotipo

# 1. Crear la variable CS1_5 (freezing temprano)
df3$CS1_5 <- rowMeans(df3[, c("CS1", "CS2", "CS3", "CS4", "CS5")], na.rm = TRUE)

# 2. Verificar que se creó correctamente
print(names(df3))

# 3. Boxplot de Freezing CS1–5 por sexo, añadiendo el genotipo como color
library(ggplot2)

ggplot(df3, aes(x = Sex, y = CS1_5)) +
  geom_boxplot(alpha = 0.4, aes(fill = Sex)) +
  geom_jitter(aes(color = Genotype), width = 0.2, size = 2) +
  labs(title = "Freezing temprano (CS1–5) por sexo y genotipo",
       y = "Freezing CS1–5 (%)",
       x = "Sexo",
       color = "Genotipo") +
  theme_bw()

# Boxplot CeM por sexo y genotipo
ggplot(df_merge, aes(x = Sex.x, y = CeM)) +
  geom_boxplot(alpha = 0.4, aes(fill = Sex.x)) +
  geom_jitter(aes(color = Genotype.x), width = 0.2, size = 2) +
  labs(title = "Activación neuronal en CeM por sexo y genotipo",
       y = "cFos en CeM",
       x = "Sexo",
       color = "Genotipo") +
  theme_bw()

# ML
# Modelo predictivo sencillo: CeM ~ Freezing_CS1_5. Predice la activación neuronal en CeM usando el freezing temprano.
modelo_simple <- lm(CeM ~ Freezing_CS1_5, data = df_merge)
summary(modelo_simple)

# Gráfico con la predicción
ggplot(df_merge, aes(x = Freezing_CS1_5, y = CeM)) +
  geom_point(aes(color = Sex.x), size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Modelo predictivo: Freezing (CS1–5) → Activación en CeM",
    x = "Freezing promedio CS1–5 (%)",
    y = "cFos en CeM"
  ) +
  theme_bw()

# Diagnóstico del modelo:Linealidad + outliers + influencia
par(mfrow = c(2, 2))
plot(modelo_simple)
par(mfrow = c(1, 1))

# Evaluar normalidad:
# Histograma de residuos
hist(modelo_simple$residuals,
     main = "Distribución de residuos",
     xlab = "Residuos",
     col = "lightblue")

# QQPLOT
qqPlot(modelo_simple, main = "QQ Plot de los residuos")

# Homocedasticidad (varianza constante)
library(lmtest)
bptest(modelo_simple)

# Independencia de errores
dwtest(modelo_simple)
