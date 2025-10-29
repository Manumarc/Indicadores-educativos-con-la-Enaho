# Paquetes requeridos

- library(dplyr)
- library(readr)
- library(purrr)
- library(fs)

# Indicadores educativos con la Enaho
Conjunto de funciones para el procesamiento y cálculo de indicadores educativos con las bases de datos de la Enaho.

## Llamar a las funciones 

Para llamar a las funciones desarrolladas se debe usar el siguiente código:

```
devtools::source_url("https://raw.githubusercontent.com/Manumarc/Indicadores-censo-escolar/refs/heads/main/funcionesce.R")
```

## Descargar bases

La función "descargar_bases" permite descargar las bases del Censo Escolar para los años 2022, 2023 y/o 2024 de manera automática. Además, las almacena en el entorno del proyecto de Rmarkdown dentro de una carpeta llamada "01 Bases" de manera automática. La función puede descargar las bases de cada año por separado o en conjunto según se requiera. A continuación se muestra la forma en que se usa el código.

```
# Llamar las bases del año 2023 #
#-------------------------------#

descargar_bases("Enaho",c("2023"))

