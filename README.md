# Paquetes requeridos

- library(tidyverse) # Un conjunto de paquetes interrelacionados diseñados para análisis de datos
- library(foreign) # Importar y exportar bases de spss
- library(purrr) # Facilita la programación funcional en R (uso de map)
- library(survey)  # Permite analizar datos provenientes de muestras complejas
- library(fs) # Simplifica y agiliza la manipulación de archivos y directorios en R

# Indicadores educativos con la Enaho
Conjunto de funciones para el procesamiento y cálculo de indicadores educativos con las bases de datos de la Encuesta nacional de hogares (Enaho) y la Encuesta demográfica y de salud (Endes).

## Llamar a las funciones 

Para llamar a las funciones desarrolladas se debe usar el siguiente código:

```
devtools::source_url("https://raw.githubusercontent.com/Manumarc/Indicadores-educativos-con-la-Enaho/refs/heads/main/funciones_enaho.R")
```
## Información de las encuestas

La función "info.encuesta" permite visualizar la información de módulos que hay para uno o varios años de la Enaho o la Endes.

```
#--------------------------------------------------------------------------#
# Módulos que hay en la Endes de 2023 y 2024
#--------------------------------------------------------------------------#

info.encuesta("Endes",c(2023,2024))

```

## Descargar bases

La función "descargar_bases" permite descargar las bases de la Enaho o la Endes para los años 2016 en adelante de manera automática. Además, las almacena en el entorno del proyecto de Rmarkdown dentro de una carpeta llamada "01 Bases" de manera automática. La función puede descargar las bases de cada año por separado o en conjunto según se requiera. A continuación se muestra la forma en que se usa el código.

```
#--------------------------------------------------------------------------#
# Descargar Enaho del año 2023, los módulos 1, 3 y 34
#--------------------------------------------------------------------------#

descargar_bases("Enaho",c(2023),c("01","03","34"))

#--------------------------------------------------------------------------------#
# Descargar Enaho del año 2006 al 2023, los módulos 1629, 1630, 1638, 64, 65, 74
#--------------------------------------------------------------------------------#

# descargar_bases("Endes",c(2006:2024),c("1629","1630","1638","64","65","74"))

```
