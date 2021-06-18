# Desigualdad de género en Córdoba
En este repositorio vamos a presentar los principales indicadores del mercado laboral para el Gran Córdoba en clave de género. Como ya sabemos, las estadísticas oficiales (todavía) no abarcan las diversidades de identidades de género, por lo que se distibuyen los datos de manera binaria en "Mujer - Varon". A pesar de esta limitación, es interesante hechar luz sobre estos indicadores por dos motivos: la falta de procesamiento y divulgación de este tipo de información sobre este territorio en particular y la necesidad de aportar evidencia empírica de la desigualdad que enfrentan las mujeres en diversos ámbitos.

## Armado de la base de datos

Por empezar, llamamos a las librerías que vamos a utilizar.
```
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(plotly)
library(eph)
```
Con el paquete de EPH vamos a juntar los datos de los trimestres 1,2,3 y 4 para los años 2018 y 2019, y de los trimestres 1, 2 para el año 2020. 
Luego unimos esas bases, agregando una variable que indique el `periodo` de los datos, filtramos por el `AGLOMERADO` Córdoba y asignamos nombre a la variable `Sexo` y a sus valores `Varon` `Mujer` 
```
individual <- get_microdata(
  year = 2018:2019,
  trimester = c(1,2,3,4),
  type = "individual",
  vars = "all")
individual <- individual %>% 
  select(microdata) %>% 
  unnest(microdata) %>% 
  
individual20 <- get_microdata(
    year = 2020,
    trimester = c(1,2),
    type = "individual",
    vars = "all")
individual20 <- individual20 %>% 
  select(microdata) %>% 
  unnest(microdata)

Union_Bases <- bind_rows(individual, 
                         individual20) %>%
filter(AGLOMERADO==13) %>% 
mutate(periodo = paste(ANO4, TRIMESTRE, sep = "t")) %>% 
mutate(Sexo = case_when(CH04 == 1 ~ "Varon",
                        CH04 == 2 ~ "Mujer"))

summary(Union_Bases$ANO4)
summary(Union_Bases$TRIMESTRE)
```

## Armado de los indicadores

Realizamos el cálculo de indicadores: 
- Tasa de Actividad
- Tasa de Empleo
- Tasa de Desocupación
- Tasa de Subocupación
- Tasa de Subocupación demandante
- Tasa de Subocupación no demandante.
```
tabla1 <- Union_Bases %>% 
  filter(CH06 >= 14) %>% 
  group_by(Sexo, periodo) %>% 
  summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]),
            Desocupados       = sum(PONDERA[ESTADO == 2]),
            PEA               = Ocupados + Desocupados,
            Ocupados_demand   = sum(PONDERA[ESTADO == 1 & PP03J ==1]),
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            Subocupados       = Suboc_demandante + Suboc_no_demand) %>% 
  mutate(  "Tasa Actividad" = round(PEA/Poblacion, 3),
           "Tasa Empleo"                     = round(Ocupados/Poblacion, 3),
           "Tasa Desocupación"               = round(Desocupados/PEA, 3),
           "Tasa Ocupados Demandantes"       = round(Ocupados_demand/PEA, 3),
           "Tasa Subocupación"              = round(Subocupados/PEA, 3),
           "Tasa Subocupación demandante"    = round(Suboc_demandante/PEA, 3),
           "Tasa Subocupación no demandante" = round(Suboc_no_demand/PEA, 3)) 
```
Reordenamos los datos con `gather` y modificamos los valores de la variable `Indicadores` dependiendo del sexo de la persona
```
tabla2 <- tabla1 %>% 
  gather(Indicadores, proporcion,11:17) %>% 
  select (Sexo,periodo, Indicadores, proporcion) %>%
  mutate( Indicadores = case_when(Sexo=="Varon" & Indicadores=="Tasa Actividad" ~ "Tasa Actividad varones",
                                  Sexo=="Mujer" &	Indicadores=="Tasa Actividad" ~ "Tasa Actividad mujeres",
                                  Sexo=="Varon" &	Indicadores=="Tasa Empleo" ~ "Tasa Empleo varones",
                                  Sexo=="Mujer" &	Indicadores=="Tasa Empleo" ~ "Tasa Empleo mujeres",
                                  Sexo=="Varon" &	Indicadores=="Tasa Desocupación" ~ "Tasa Desocupación varones",
                                  Sexo=="Mujer" &	Indicadores=="Tasa Desocupación" ~ "Tasa Desocupación mujeres",
                                  Sexo=="Varon" &	Indicadores=="Tasa Subocupación" ~ "Tasa Subocupación varones",
                                  Sexo=="Mujer" &	Indicadores=="Tasa Subocupación" ~ "Tasa Subocupación mujeres",
                                  Sexo=="Varon" &	Indicadores=="Tasa Ocupados Demandantes" ~ "Tasa Ocupados Demandantes varones",
                                  Sexo=="Mujer" &	Indicadores=="Tasa Ocupados Demandantes" ~ "Tasa Ocupados Demandantes mujeres",
                                  Sexo=="Varon" &	Indicadores=="Tasa Subocupación demandante" ~ "Tasa Subocupación demandante varones",
                                  Sexo=="Mujer" &	Indicadores=="Tasa Subocupación demandante" ~ "Tasa Subocupación demandante mujeres",
                                  Sexo=="Varon" &	Indicadores=="Tasa Subocupación no demandante" ~ "Tasa Subocupación no demandante varones",
                                  Sexo=="Mujer" &	Indicadores=="Tasa Subocupación no demandante" ~ "Tasa Subocupación no demandante mujeres"))
```
## Visualización interactiva de los Indicadores

Separamos los indicadores en dos gráficos interactivos creados con `plotly`

```
grafico1 <- tabla2 %>% 
  filter(Indicadores=="Tasa Actividad mujeres"|
         Indicadores=="Tasa Actividad varones"|
         Indicadores=="Tasa Empleo varones"|
         Indicadores=="Tasa Empleo mujeres"|
         Indicadores=="Tasa Desocupación varones"|
         Indicadores=="Tasa Desocupación mujeres"|
         Indicadores=="Tasa Ocupados Demandantes varones"|
         Indicadores=="Tasa Ocupados Demandantes mujeres") %>% 
  ggplot(aes(x = periodo, 
             y = proporcion, 
             group = Indicadores,  
             shape = Sexo)) +
  labs(title = "Indicadores mercado laboral Gran Córdoba",
       x = "Periodo",
       y = "porcentaje")+
  geom_line(aes(color = Indicadores),
                size = 0.7) +
  geom_text(aes(label = sprintf("%1.1f%%", 100*proporcion),
                col= Indicadores),
                alpha=0.7,position = position_nudge(y = 0.02)) +
  geom_point(aes(colour = Indicadores),
             size = 2) +
  scale_color_brewer(palette = "PuOr")+
    theme_minimal()

ggplotly(grafico1)

grafico2 <- tabla2 %>% 
  filter(Indicadores=="Tasa Subocupación mujeres"|
           Indicadores=="Tasa Subocupación varones"|
           Indicadores=="Tasa Subocupación demandante varones"|
           Indicadores=="Tasa Subocupación demandante mujeres"|
           Indicadores=="Tasa Subocupación no demandante varones"|
           Indicadores=="Tasa Subocupación no demandante mujeres") %>% 
  ggplot(aes(x = periodo, 
             y = proporcion, 
             group = Indicadores,  
             shape = Sexo)) +
  labs(title = "Indicadores mercado laboral Gran Córdoba",
       x = "Periodo",
       y = "porcentaje")+
  geom_line(aes(color = Indicadores),
            size = 0.7) +
  geom_text(aes(label = sprintf("%1.1f%%", 100*proporcion),
                col= Indicadores),
            alpha=0.7,position = position_nudge(y = 0.02)) +
  geom_point(aes(colour = Indicadores),
             size = 2) +
  scale_color_brewer(palette = "PuOr")+
  theme_minimal()

ggplotly(grafico2)

```
