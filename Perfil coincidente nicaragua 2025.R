##===========================================================================##
## Objetivo: Estimación del perfil coincidente - inflación instantanea NiC 2025
## Fecha: Julio 2025
##===========================================================================##


##===========================================================================##
## Paso 1: Limpiar espacio de trabajo e instalar paquetes                    ##
##===========================================================================##

rm(list = ls())
graphics.off()
options(encoding = "UTF-8")
Sys.setlocale("LC_ALL", "en_US.UTF-8")

install.packages("Coinprofile")
install.packages("zoo")
install.packages("stats")
install.packages("plyr")
# install.packages("coin")
# install.packages("Rdpack")
# install.packages("exactRankTests")
install.packages("ggplot2")

library(Coinprofile)
library(zoo)
library(stats)
library(plyr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)

##===========================================================================##
## Paso 2: Importar las series
##===========================================================================##

inf_instxl <- read_excel("C:/Users/Hp/OneDrive/Escritorio/Inflación instantanea/Perfil coincidente infinst nic/Inflación instantanea base limpia.xlsx")


df_infinst <- inf_instxl %>%
  mutate(
    year = as.numeric(substr(Fecha, 1, 4)),
    month = as.numeric(substr(Fecha, 6, 7)),
    date = as.Date(paste0(year, "-", month, "-01")),
    quarter = quarter(date)
  )


df_infinst <- df_infinst %>%
  filter(
    year(date) != 2011 |        # mantener todo lo que no sea 2011
      !(                      # o si es 2011, solo mantener si NO son todos NA
        is.na(INFLATION_A0) &
          is.na(INFLATION_A1) &
          is.na(INFLATION_A2) &
          is.na(INFLATION_A3)
      )
  )

##===========================================================================##
## Paso 4: Obtener el perfil coincidente
##===========================================================================##

coinA0 <- coincident_profile(df_infinst$INFLATION_A0, df_infinst$INFLATION_CONVENCIONAL, 
                             12, 13, "Inflanción instantánea a=0", "Inflación interanual", 
                             TRUE, 2012, 2025, 12, 6, "Inflación instantánea a=0", "Inflación interanual", 
                             "Inflación instantánea a=0 e Inflación interanual")

coinA1 <- coincident_profile(df_infinst$INFLATION_A1, df_infinst$INFLATION_CONVENCIONAL, 12, 13, "Inflanción instantánea a=1", "Inflación interanual", TRUE, 2012, 2025, 12, 6, "Inflación instantánea a=1", "Inflación interanual", "Inflación instantánea a=1 e Inflación interanual")

coinA2 <- coincident_profile(df_infinst$INFLATION_A2, df_infinst$INFLATION_CONVENCIONAL, 12, 13, "Inflanción instantánea a=2", "Inflación interanual", TRUE, 2012, 2025, 12, 6, "Inflación instantánea a=2", "Inflación interanual", "Inflación instantánea a=2 e Inflación interanual")

coinA3 <- coincident_profile(df_infinst$INFLATION_A3, df_infinst$INFLATION_CONVENCIONAL, 12, 13, "Inflanción instantánea a=3", "Inflación interanual", TRUE, 2012, 2025, 12, 6, "Inflación instantánea a=3", "Inflación interanual", "Inflación instantánea a=3 e Inflación interanual")

coinA4 <- coincident_profile(df_infinst$INFLATION_A4, df_infinst$INFLATION_CONVENCIONAL, 12, 13, "Inflanción instantánea a=4", "Inflación interanual", TRUE, 2012, 2025, 12, 6, "Inflación instantánea a=4", "Inflación interanual", "Inflación instantánea a=4 e Inflación interanual")

str(coinA0)

##===========================================================================##
## Paso 5: Graficar los cinco perfiles
##===========================================================================##
dfA0 <- cbind(coinA0$Profile, a = "a = 0")
dfA1 <- cbind(coinA1$Profile, a = "a = 1")
dfA2 <- cbind(coinA2$Profile, a = "a = 2")
dfA3 <- cbind(coinA3$Profile, a = "a = 3")
dfA4 <- cbind(coinA4$Profile, a = "a = 4")

df_all <- bind_rows(dfA0, dfA1, dfA2, dfA3, dfA4)

df_all$a <- factor(df_all$a, 
                   levels = c("a = 0", "a = 1", "a = 2", "a = 3", "a = 4"))

df_all <- df_all %>%
  mutate(p_norm = p.value / 100)  

ggplot(df_all, aes(x = lags, y = p_norm)) +
  geom_col(fill = "gray") +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
  facet_wrap(~ a, nrow = 3) + 
  labs(
    title = "",
    x = "Lag",
    y = "P-value"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 9, face = "italic"),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
