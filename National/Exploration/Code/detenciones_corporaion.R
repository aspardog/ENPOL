# Loading data
load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData"))

Detenciones_Uso_Fuerza  <- read_excel(paste0(path2SP,"/National/Exploration/Input/Politica_criminal/Detenciones-Uso_Fuerza.xlsx"))

master_data.df <- Main_database %>% 
  filter(Anio_arresto >= as.numeric(2008)) %>% 
  filter(NSJP == 1) 

detenciones <- Detenciones_Uso_Fuerza %>%
  filter(Categoria == "Detenciones") %>%
  select(`Tipo de policía`, det_21 = `2021`, det_20 = `2020`) %>%
  mutate(
    `Tipo de policía` = if_else(`Tipo de policía` == "Policías Estatales por puesta a disposición",
                                "Policía Estatal", `Tipo de policía`)
  ) %>%
  filter(`Tipo de policía` %in% c("Policía Estatal", "Policía Municipal", "Guardia Nacional")) %>%
  rowwise() %>%
  mutate(
    total = sum(as.numeric(det_21), as.numeric(det_20))
    ) %>%
  select(Corporacion_grupos = `Tipo de policía`, total)
  

personas_ENPOL <- master_data.df %>%
  filter(Anio_arresto > 2019) %>%
  group_by(Corporacion_grupos) %>%
  summarise(
    personas_ENPOL = n()
  ) %>%
  filter(Corporacion_grupos %in% c("Policía Estatal", "Policía Municipal", "Guardia Nacional"))
  
personas_ENPOL <- master_data.df %>%
  filter(Anio_arresto > 2019) %>%
  group_by(Corporacion_grupos, Delito_unico_categ) %>%
  summarise(
    personas_ENPOL = n()
  ) %>%
  filter(Corporacion_grupos %in% c("Policía Estatal", "Policía Municipal", "Guardia Nacional")) %>%
  drop_na() %>%
  arrange(Corporacion_grupos, -personas_ENPOL) %>%
  group_by(Corporacion_grupos) %>%
  mutate(
    total = sum(personas_ENPOL),
    prop = personas_ENPOL/total*100
  )



personas_ENPOL_fuero <- master_data.df %>%
  filter(Anio_arresto > 2019) %>%
  group_by(Corporacion_grupos, fuero) %>%
  summarise(
    personas_ENPOL = n()
  )%>%
  filter(Corporacion_grupos %in% c("Policía Estatal", "Policía Municipal", "Guardia Nacional")) %>%
  group_by(Corporacion_grupos) %>%
  mutate(
    total = sum(personas_ENPOL),
    proporcion_fuero = personas_ENPOL/total
  ) %>%
  select(Corporacion_grupos, fuero, proporcion_fuero) %>%
  pivot_wider(id_cols = Corporacion_grupos, names_from = fuero, values_from = proporcion_fuero)


detenciones_ENPOL <- master_data.df %>%
  filter(Anio_arresto > 2019) %>%
  group_by(Corporacion_grupos) %>%
  summarise(
    det_ninguna = mean(det_ninguna, na.rm = T),
    flagrancia  = mean(flagrancia, na.rm = T),
    orden_det   = mean(orden_det, na.rm = T),
    inspeccion  = mean(inspeccion, na.rm = T)
  ) %>%
  filter(Corporacion_grupos != "NS/NR") %>%
  filter(Corporacion_grupos %in% c("Policía Estatal", "Policía Municipal", "Guardia Nacional"))

detenciones_final <- detenciones %>%
  left_join(personas_ENPOL, by = "Corporacion_grupos") %>%
  left_join(detenciones_ENPOL, by = "Corporacion_grupos") %>%
  left_join(personas_ENPOL_fuero, by = "Corporacion_grupos") %>%
  left_join(personas_ENPOL_2, by = "Corporacion_grupos")

write.xlsx(detenciones_final, 
           paste0(path2SP,"/National/Exploration/Input/Politica_criminal/detenciones_corporacion.xlsx"))
  
  