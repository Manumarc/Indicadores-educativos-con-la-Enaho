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

La función "descargar_bases" permite descargar las bases de la Enaho o la Endes para los años 2016 en adelante de manera automática. Además, las almacena en el entorno del proyecto de Rmarkdown dentro de una carpeta llamada "01 Bases" de manera automática. La función puede descargar las bases de cada año por separado o en conjunto según se requiera. A continuación se muestra la forma en que se usa el código.

```
#--------------------------------------------------------------------------#
# Descargar Enaho del año 2023, los módulos 1, 3 y 34
#--------------------------------------------------------------------------#

descargar_bases("Enaho",c(2023),c(1,3,34)

```
