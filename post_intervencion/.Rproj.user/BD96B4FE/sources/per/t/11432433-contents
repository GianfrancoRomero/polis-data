# Librerías
library(rio)
library(tidyverse)

# Rango de años
anios_int <- tibble(
  code_pais = c("IRQ", "LBY", "PAN", "AFG"),
  t= c(2003, 2011, 1989, 2001) # t = Año de intervención
)

############################# PBI per cápita (US$) #############################

gdp_data = readxl::read_excel("API_NY.GDP.PCAP.CD_DS2_en_excel_v2_281.xls",
                              sheet = "Data")

# Preparar estructura de data
gdp_names = gdp_data[3,]
gdp_data = gdp_data[-(1:3),] # Filas innecesarias
colnames(gdp_data) = gdp_names
gdp_data = gdp_data[,-(3:4)] # Columnas innecesarias

# Aplicamos pivot_long
gdp_long <- gdp_data %>%
  pivot_longer(
    cols = -c(`Country Name`, `Country Code`),
    names_to = "Year",
    values_to = "GDP"
  )

gdp_long$GDP = as.numeric(gdp_long$GDP)

# Países y años seleccionados
gdp_clean <- gdp_long %>%
  inner_join(
    anios_int,
    by = c("Country Code" = "code_pais")
    ) %>%
  filter(
    as.numeric(Year) >= t - 10,
    as.numeric(Year) <= t + 10
    ) %>%
  mutate(t_event = as.numeric(Year) - t) %>%
  select(-t)

# Exportar data lista
write.csv(gdp_clean, "gdp_clean.csv", row.names = FALSE)

########################## Índice de Capacidad Estatal ##########################

sci_data = read.csv("state-capacity-index.csv")

# Normalizamos el índice en una escala de 0 a 1
sci_data <- sci_data %>% 
  mutate(
    norm_sci = (State.Capacity.Index - min(State.Capacity.Index)) /
      (max(State.Capacity.Index) - min(State.Capacity.Index))
    )

# Países y años seleccionados
sci_clean <- sci_data %>%
  inner_join(
    anios_int,
    by = c("Code" = "code_pais")
  ) %>%
  filter(
    as.numeric(Year) >= t - 10,
    as.numeric(Year) <= t + 10
  ) %>%
  mutate(t_event = as.numeric(Year) - t) %>%
  select(-t)

# Exportar data lista
write.csv(sci_clean, "sci_clean.csv", row.names = FALSE)

######################### Índice de Democracia Liberal #########################

dem_data = read.csv("Liberal Democracy Index.csv")

dem_data <- dem_data %>% 
  select(Year, Afghanistan, Iraq, Libya, Panama)

dem_long <- dem_data %>%
  pivot_longer(
    cols = -Year,
    names_to = "Country",
    values_to = "LD_Index",
    values_transform = list(LD_Index = as.numeric)
  )

# Preparamos años t para esta data
anios_int <- tibble(
  pais = c("Iraq", "Libya", "Panama", "Afghanistan"),
  t= c(2003, 2011, 1989, 2001) # t = Año de intervención
)

# Países y años seleccionados
dem_clean <- dem_long %>%
  inner_join(
    anios_int,
    by = c("Country" = "pais")
  ) %>%
  filter(
    as.numeric(Year) >= t - 10,
    as.numeric(Year) <= t + 10
  ) %>%
  mutate(t_event = as.numeric(Year) - t) %>%
  select(-t)

# Exportar data lista
write.csv(dem_clean, "dem_clean.csv", row.names = FALSE)













