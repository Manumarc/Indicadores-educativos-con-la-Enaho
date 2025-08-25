#===========================================================#
# Base de datos con nombre de regiones y ubigeo a 2 dígitos #
#===========================================================#

# Lima incluye a Lima Provincias y Lima Metropolitana

bd_regiones <- data.frame(
  ubigeo_reg = sprintf("%02d", 1:25),
  nom_region = c(
    "Amazonas", "Áncash", "Apurímac", "Arequipa", "Ayacucho",
    "Cajamarca", "Callao", "Cusco", "Huancavelica", "Huánuco",
    "Ica", "Junín", "La Libertad", "Lambayeque", "Lima",
    "Loreto", "Madre de Dios", "Moquegua", "Pasco", "Piura",
    "Puno", "San Martín", "Tacna", "Tumbes", "Ucayali"
  ),
  stringsAsFactors = FALSE
)

#=================================================================#
# Función para descargar las bases de la Enaho por módulos y años #
#=================================================================#

descargar_bases <- function(nom_encuesta, años, modulos){

  carpeta_destino <- "01 Bases"
  if (!dir.exists(carpeta_destino)) dir.create(carpeta_destino, recursive = TRUE)

  # URL y destino del CSV
  url <- "https://raw.githubusercontent.com/Manumarc/Indicadores-educativos-con-la-Enaho/refs/heads/main/Descarga_enaho.csv"
  destino <- file.path(tempdir(), "Descarga_enaho.csv")  # evita problemas de permisos

  # Descargar el CSV
  download.file(url, destfile = destino, mode = "wb")

  descargar <- read_csv2(destino, show_col_types = FALSE)

  # Filtrar según input
  df_filtrado <- descargar %>%
    filter(encuesta %in% nom_encuesta) %>%
    filter(año %in% años) %>%
    filter(num_modulo %in% modulos)

  # Descarga, descompresión y copia del .sav
  walk2(seq_len(nrow(df_filtrado)), paste0(nom_encuesta, "_", df_filtrado$año, "_modulo_", df_filtrado$num_modulo), function(i, nombre) {

    url <- df_filtrado$link[i]
    nom_bds <- df_filtrado$nom_bd[i]

    ruta_zip <- file.path(carpeta_destino, paste0(nombre, ".zip"))
    carpeta_temporal <- file.path(carpeta_destino, nombre)

    message("Descargando ", nombre, "...")
    download.file(url, destfile = ruta_zip, mode = "wb")

    message("Descomprimiendo ", nombre, "...")
    unzip(ruta_zip, exdir = carpeta_temporal)

    # Separar los nombres de archivo (si hay varios)
    archivos_objetivo <- strsplit(nom_bds, ",\\s*")[[1]]

    # Iterar sobre cada archivo .sav a buscar
    for (archivo_nom in archivos_objetivo) {
  
      archivo_base <- archivo_nom   # aquí pones "Enaho01-2024-100"
      
      todos_archivos <- list.files(
        path = carpeta_temporal,
        recursive = TRUE,
        full.names = TRUE
      )
      
      archivo_encontrado <- todos_archivos[grepl(archivo_base, basename(todos_archivos), ignore.case = TRUE)]
      
      if (length(archivo_encontrado) > 0) {
        # le forzamos la extensión .sav al copiar
        destino_sav <- file.path(carpeta_destino, paste0(basename(archivo_encontrado)))
        fs::file_copy(archivo_encontrado, destino_sav, overwrite = TRUE)
        message("Archivo copiado a: ", destino_sav)
      } else {
        warning("No se encontró archivo: ", archivo_nom, " en ", carpeta_temporal)
      }
    }
  })

  message("Proceso completo.")
}

#==============================================================================#
# Función para el cálculo de total de años de escolaridad según rangos de edad #
#==============================================================================#

tescola <- function(mod_salud,mod_edu,mod_sumaria, rango_edad){
  
  nom_año <- mod_salud %>% 
    dplyr::select(AÑO) %>% 
    unique() %>% 
    pull()
  
  # Integración de bases de datos #
  #-------------------------------#
  
  # Definir llaves para cada pegado de bases de datos #
  
  claves_educ <- c("CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO")
  
  claves_sumaria <- c("CONGLOME", "VIVIENDA", "HOGAR")
  
  bdedu_filtrado <- mod_edu %>%
    dplyr::select(all_of(claves_educ), setdiff(names(.), union(names(mod_salud), claves_educ)))
  
  # Pegar base de educación
  
  bdtemp <- mod_salud %>%
    left_join(bdedu_filtrado, by = claves_educ)
  
  # Filtrar columnas repetidas y únicas de la base de sumaria 
  
  bdsumaria_filtrada <- mod_sumaria %>%
    dplyr::select(all_of(claves_sumaria), setdiff(names(.), union(names(bdtemp), claves_sumaria)))
  
  # Pegar base de sumaria
  
  bd_int <- bdtemp %>%
    left_join(bdsumaria_filtrada, by = claves_sumaria)
  
  # Preparación de variables #
  #--------------------------#
  
  a1 <- bd_int %>% 
    mutate(area = case_when(ESTRATO %in% c(1:5) ~ "Urbana",
                            TRUE ~ "Rural")
    ) %>%  # Conversión de niveles educativos a años de escolaridad
    mutate(x5 = P301B) %>%  
    mutate(x5 = case_when(P301A %in% c(1,2) ~ 0,
                          P301A %in% 5 & x5 %in% 1 ~ 7, 
                          P301A %in% 5 & x5 %in% 2 ~ 8,
                          P301A %in% 5 & x5 %in% 3 ~ 9,
                          P301A %in% 5 & x5 %in% 4 ~ 10,
                          P301A %in% 6 & x5 %in% 5 ~ 11,
                          P301A %in% 6 & x5 %in% 6 ~ 12,
                          P301A %in% 7 & x5 %in% 1 ~ 12,
                          P301A %in% 7 & x5 %in% 2 ~ 13,
                          P301A %in% 7 & x5 %in% 3 ~ 14,
                          P301A %in% 7 & x5 %in% 4 ~ 15,
                          P301A %in% 8 & x5 %in% 3 ~ 14,
                          P301A %in% 8 & x5 %in% 4 ~ 15,
                          P301A %in% 8 & x5 %in% 5 ~ 16,
                          P301A %in% 9 & x5 %in% 1 ~ 12,
                          P301A %in% 9 & x5 %in% 2 ~ 13,
                          P301A %in% 9 & x5 %in% 3 ~ 14,
                          P301A %in% 9 & x5 %in% 4 ~ 15,
                          P301A %in% 9 & x5 %in% 5 ~ 16,
                          P301A %in% 9 & x5 %in% 6 ~ 17,
                          P301A %in% 10 & x5 %in% 4 ~ 15,
                          P301A %in% 10 & x5 %in% 5 ~ 16,
                          P301A %in% 10 & x5 %in% 6 ~ 17,
                          P301A %in% 10 & x5 %in% 7 ~ 18,
                          P301A %in% 11 & x5 %in% 1 ~ 17,
                          P301A %in% 11 & x5 %in% 2 ~ 18,
                          TRUE ~ x5)) %>% 
    mutate(t_p301c = P301C) %>% 
    mutate(t_p301c = case_when(t_p301c %in% 0 ~ 1,
                               TRUE ~ t_p301c)) %>% 
    mutate(x5 = case_when(P301B %in% 0 & !P301A %in% 2 ~ t_p301c,
                          TRUE ~ x5)) %>% 
    mutate(AÑO = as.numeric(AÑO)) %>% 
    mutate(edad31m = AÑO - P400A3) %>% 
    mutate(edad31m = case_when(P400A2 > 3 ~ (AÑO - P400A3 - 1),
                               TRUE ~ edad31m)) %>% 
    mutate(edad31m = case_when(edad31m < 0 ~ 0,
                               TRUE ~ edad31m)) %>% 
    mutate(escol = case_when(edad31m >= rango_edad[[1]] & edad31m <= rango_edad[[2]] ~ "1",
                             TRUE ~ "0")) %>%  # Identificador de nombre de regiones (26)
    mutate(cod_reg = substr(UBIGEO,1,2), 
           cod_limamet = substr(UBIGEO,1,4)) %>% 
    mutate(region = case_when(cod_reg %in% "01" ~ "Amazonas",
                              cod_reg %in% "02" ~ "Áncash",
                              cod_reg %in% "03" ~ "Apurímac",
                              cod_reg %in% "04" ~ "Arequipa",
                              cod_reg %in% "05" ~ "Ayacucho",
                              cod_reg %in% "06" ~ "Cajamarca",
                              cod_reg %in% "07" ~ "Callao",
                              cod_reg %in% "08" ~ "Cusco",
                              cod_reg %in% "09" ~ "Huancavelica",
                              cod_reg %in% "10" ~ "Huánuco",
                              cod_reg %in% "11" ~ "Ica",
                              cod_reg %in% "12" ~ "Junín",
                              cod_reg %in% "13" ~ "La Libertad",
                              cod_reg %in% "14" ~ "Lambayeque",
                              cod_reg %in% "15" ~ "Lima Provincias",
                              cod_reg %in% "16" ~ "Loreto",
                              cod_reg %in% "17" ~ "Madre de Dios",
                              cod_reg %in% "18" ~ "Moquegua",
                              cod_reg %in% "19" ~ "Pasco",
                              cod_reg %in% "20" ~ "Piura",
                              cod_reg %in% "21" ~ "Puno",
                              cod_reg %in% "22" ~ "San Martín",
                              cod_reg %in% "23" ~ "Tacna",
                              cod_reg %in% "24" ~ "Tumbes",
                              cod_reg %in% "25" ~ "Ucayali",
                              TRUE ~ NA_character_
    )) %>% 
    mutate(region = case_when(cod_limamet == "1501" ~ "Lima Metropolitana",
                              TRUE ~ region))
  
  # Cálculo de años de escolaridad por región #
  #-------------------------------------------#
  
  bd_a1 <- a1 %>% 
    mutate(año = nom_año) %>% 
    group_by(año,region,area,escol) %>% 
    summarise(x5 = redondear(weighted_mean(x5, FACTORA07, na.rm = TRUE),0)) %>% 
    filter(escol %in% 1) %>% 
    dplyr::select(-escol)
  
  return(bd_a1)
  
}
