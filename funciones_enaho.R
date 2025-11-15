#========================================================#
# Creación de base de datos de Enaho y Endes 2006 a 2024 #
#========================================================#

objetos_antes <- ls()

# Vector de módulos enaho #
#-------------------------#

# Módulos de la Enaho
mod_enaho <- c("01","02","03","04","05","07","08","09","10","11","12","13","15","16","17","18","22","23","24","25","26","27","28","34","37","77","78","84","85","1825")

# Módulos de la Endes
mod_endes1 <- c("1629","1630","1631","1632","1633","1634","1635","1636","1637","1638","1639","1640","1641")
mod_endes2<- c("64","65","66","67","69","70","71","72","73","74","413","414","569")

#Vector de nombre de los módulos #
#--------------------------------#

nom_modulo_enaho <- c("Características de la vivienda y del hogar",
                      "Características de los miembros del hogar",
                      "Educación",
                      "Salud",
                      "Empleo e ingresos",
                      "Gastos en alimentos y bebidas (módulo 601)",
                      "Instituciones beneficas",
                      "Mantenimiento de la vivienda",
                      "Transportes y Comunicaciones",
                      "Servicios a la vivienda",
                      "Esparcimiento, diversion y servicios de cultura",
                      "Vestido y calzado",
                      "Gastos de transferencias",
                      "Muebles y enseres",
                      "Otros bienes y servicios",
                      "Equipamiento del hogar",
                      "Producción agrícola",
                      "Subproductos agricolas",
                      "Producción forestal",
                      "Gastos en actividades agricolas y/o forestales",
                      "Producción pecuaria",
                      "Subproductos pecuarios",
                      "Gastos en actividades pecuarias",
                      "Sumarias (variables calculadas)",
                      "Programas sociales (miembros del hogar)",
                      "Ingresos del trabajador independiente",
                      "Bienes y Servicios de Cuidados Personales",
                      "Participación Ciudadana",
                      "Gobernabilidad, Democracia y Transparencia",
                      "Beneficiarios de Instituciones sin fines de lucro: Olla Común")

nom_modulo_endes <- c("Caracteristicas del Hogar",
                      "Caracteristicas de la Vivienda",
                      "Datos Basicos de MEF",
                      "Historia de Nacimiento - Tabla de Conocimiento de Metodo",
                      "Embarazo, Parto, Puerperio y Lactancia",
                      "Inmunización y Salud",
                      "Nupcialidad - Fecundidad - Cónyugue y Mujer",
                      "Conocimiento de Sida y uso del condón",
                      "Mortalidad Materna - Violencia Familiar",
                      "Peso y talla - Anemia",
                      "Disciplina Infantil",
                      "Encuesta de salud",
                      "Programas Sociales")

# Vector de años 
num_años <- c(2006:2024)

#Vector de código de encuesta (por año) #
#---------------------------------------#

cod_encuesta_enaho <- c(282:285,279,291,324,404,440,498,546,603,634,687,737,759,784,906,966)

cod_encuesta_endes <- c(183,194,238,260,290,323,407,441,504,548,605,638,691,739,760,786,910,968)

# Construcción de base de datos Enaho #
#-------------------------------------#

# Base de datos con nombre de los módulos
tabla_nommod_enaho <- data.frame(
  modulo = mod_enaho,
  nom_modulo = nom_modulo_enaho
)

# Base de datos 
bd_inicial_enaho <- 
  expand_grid(
    encuesta = "Enaho",
    año = num_años,
    modulo = mod_enaho,
  ) %>% 
  left_join(tabla_nommod_enaho, by = "modulo") %>% 
  filter(año %in% c(2023, 2024) | modulo != "1825") %>% 
  arrange(desc(año), modulo)

# Base de datos con çodigo de encuesta para construir los links 
tabla_cod_enaho <- data.frame(
  año = num_años,
  cod = cod_encuesta_enaho
)

bd_enaho <- bd_inicial_enaho %>% 
  left_join(tabla_cod_enaho, by = "año") %>% 
  mutate(
    link = paste0(
      "https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/", cod, "-Modulo", modulo, ".zip"
    )
  ) %>% 
  dplyr::select(-cod) %>% 
  arrange(año,modulo) %>% 
  distinct()

# Construcción de base de datos Endes #
#-------------------------------------#

# Base de datos con nombre de los módulos
tabla_nommod_endes1 <- data.frame(
  modulo = mod_endes1,
  nom_modulo = nom_modulo_endes
)

tabla_nommod_endes2 <- data.frame(
  modulo = mod_endes2,
  nom_modulo = nom_modulo_endes
)

# Base de datos 

endes1 <- expand_grid(
  encuesta = "Endes",
  año = 2020:2024,
  modulo = mod_endes1,
) %>% 
  left_join(tabla_nommod_endes1, by = "modulo")

endes2 <- expand_grid(
  encuesta = "Endes",
  año = 2006:2019,
  modulo = mod_endes2,
) %>% 
  filter(!año %in% 2008) %>% 
  filter(!(año %in% c(2006:2013) & modulo %in% "569")) %>% 
  filter(!(año %in% c(2006:2012) & modulo %in% c("413","414"))) %>% 
  left_join(tabla_nommod_endes2, by = "modulo")

bd_inicial_endes <- rbind(endes1,endes2)

tabla_cod_endes <- data.frame(
  año = c(2006,2007,2009:2024),
  cod = cod_encuesta_endes
)

bd_endes <- bd_inicial_endes %>% 
  left_join(tabla_cod_endes, by = "año") %>% 
  mutate(
    link = paste0(
      "https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/", cod, "-Modulo", modulo, ".zip"
    )
  ) %>% 
  dplyr::select(-cod) %>% 
  arrange(año,modulo) %>% 
  distinct()

bd_encuestas <- rbind(bd_enaho, bd_endes)

objetos_despues <- ls()
objetos_nuevos <- setdiff(objetos_despues, objetos_antes) # Objetos creados por el script
objetos_a_borrar <- setdiff(objetos_nuevos, "bd_encuestas") # Mantener solo "bd_encuestas" entre los nuevos
rm(list = objetos_a_borrar) # Eliminar

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

#========================================================================#
# Función para descargar las bases de la Enaho y Endespor módulos y años #
#========================================================================#

descargar_bases <- function(nom_encuesta, num_años, nom_modulos) {

  #============================#
  # Crear carpeta si no existe #
  #============================#
  
  if (!dir.exists("01 Bases")) {
    dir.create("01 Bases")
  }
  
  #====================================#
  # Filtrar los links correspondientes #
  #====================================#
  
  df_descarga <- bd_encuestas %>% 
    filter(
      encuesta == nom_encuesta,
      año %in% num_años,
      modulo %in% nom_modulos
    )
  
  #=============================#
  # Proceso de descarga de .zip # 
  #=============================#
  
  for (i in seq_len(nrow(df_descarga))) {
    
    url <- df_descarga$link
    
    # Nombre con el que se guardará el zip #
    #--------------------------------------#
    
    file_out <- paste0("01 Bases/", 
                       df_descarga$encuesta[i], "_",
                       df_descarga$año[i], "_",
                       df_descarga$modulo[i], ".zip")
    
    # Descargar el .zip # 
    #-------------------#
    
    message("Descargando: ", file_out)
    
    download.file(url[i], destfile = file_out, mode = "wb")
    
  
    zip_list <- utils::unzip(file_out, list = TRUE)
    
    # Filtrar solo .sav / .SAV / .SaV (insensible a mayúsculas)
    es_sav <- grepl("(?i)\\.sav$", zip_list$Name, perl = TRUE)
    sav_en_zip <- zip_list$Name[es_sav]
    
    if (length(sav_en_zip) == 0) {
      message("  No se encontraron .sav en ", basename(file_out))
      next
    }
    
    # Descomprimir el archivo .zip #
    #------------------------------#
    
    zip::unzip(file_out, exdir = "01 Bases")   
    
    # Registrar archivos DESPUÉS de descomprimir #
    #--------------------------------------------#
    
    rutas_extraidas <- normalizePath(
      file.path("01 Bases", sav_en_zip),
      winslash = "/",
      mustWork = FALSE
    )
    
    # Filtrar solo las que realmente existen
    rutas_extraidas <- rutas_extraidas[file.exists(rutas_extraidas)]

    if (length(rutas_extraidas) == 0) {
      message("  Ningún .sav encontrado tras descomprimir: ", basename(file_out))
      next
    }
    
    # Copiar los .sav a "01 Bases" #
    #------------------------------#
    
    if (nom_encuesta == "Endes") {
      
      año_actual <- df_descarga$año[i]
      
      nombres_base <- tools::file_path_sans_ext(basename(rutas_extraidas))
      
      # Patrón: termina en "_2023" o "_2024" (o cualquier año en general)
      patron_anio <- paste0("_[0-9]{4}$")
      
      # Detectar si el nombre YA contiene año al final
      ya_tiene_anio <- grepl(patron_anio, nombres_base)
      
      # Construir los nombres finales
      nombres_finales <- ifelse(
        ya_tiene_anio,
        paste0(nombres_base, ".sav"),  # no agregar año
        paste0(nombres_base, "_", año_actual, ".sav")  # sí agregar año
      )
      
      destinos <- file.path("01 Bases", nombres_finales)
      
      file.copy(rutas_extraidas, destinos, overwrite = TRUE)
      
    } else {
      
      destinos <- file.path("01 Bases", basename(rutas_extraidas))
      file.copy(rutas_extraidas, destinos, overwrite = TRUE)
      
    }
    
    message("  Procesados ", length(rutas_extraidas), " archivos .sav.")
    
  }
  
  message("Descarga y extracción de .sav finalizada.")
  
}

#=======================================================================#
# Función para ver cuáles módulos hay en cada base en un año específico #
#=======================================================================#

info.encuesta <- function(nom_enc, nom_año) {
  
  df_filtrado <- bd_encuestas %>%
    as.data.frame() %>% 
    filter(encuesta %in% nom_enc) %>%
    filter(año %in% nom_año) %>%
    dplyr::select(encuesta, año, modulo, nom_modulo)
  
  return(df_filtrado)
  
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
