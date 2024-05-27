prueba<- Main_database %>% 
  filter(years_since_RND_3>=-2 &years_since_RND_3<=2) %>%
  group_by(RND_3, Estado_arresto) %>%
  summarise(tortura_tra_f =  mean(tortura_tra_f,na.rm=T),
            tortura_tra_p =  mean(tortura_tra_p,na.rm=T),
            tortura_mp_f =  mean(tortura_mp_f,na.rm=T),
            tortura_mp_p =  mean(tortura_mp_p,na.rm=T)) %>%
  pivot_wider(id_cols = Estado_arresto, values_from = c(tortura_tra_f,tortura_tra_p,tortura_mp_f,tortura_mp_p),names_from = RND_3) %>%
  mutate(dif_tra_f=tortura_tra_f_1-tortura_tra_f_0,
         dif_tra_p=tortura_tra_p_1-tortura_tra_p_0,
         dif_mp_f=tortura_mp_f_1-tortura_mp_f_0,
         dif_mp_p=tortura_mp_p_1-tortura_mp_p_0,)


### Pruebas logit

logit_dataBase.fn <- function(data = Main_database,
                              selectables = c("Sexo", 
                                              "Educacion_superior", 
                                              "Color_piel_claro", 
                                              "LGBTQ", 
                                              "Etnia", 
                                              "Edad_menor30"),
                              dependent_var
) {
  
  master_data.df <- data %>%
    filter(Anio_arresto >= 2008) %>%
    filter(NSJP == 1) %>%
    filter(Delito_unico == 1) %>%
    mutate(
      Educacion_superior = 
        case_when(
          Educacion_superior == 1 ~ "Cuenta con título de educación universitaria",
          Educacion_superior == 0 ~ "No cuenta con título de educación universitario",
          T ~ NA_character_
        ),
      Color_piel_claro       =
        case_when(
          Color_piel_claro      == 1 ~ "Color de piel claro",
          Color_piel_claro      == 0 ~ "Color de piel oscuro",
          T ~ NA_character_
        ),
      LGBTQ                 = 
        case_when(
          LGBTQ                 == 1 ~ "Pertenece a la comunidad LGBTQ",
          LGBTQ                 == 0 ~ "No pertenece a la comunidad LGBTQ",
          T ~ NA_character_
        ),
      Etnia                 =
        case_when(
          Etnia                 == 1 ~ "Afroamericano o indígena",
          Etnia                 == 0 ~ "No se identifica con ninguna etnia",
          T ~ NA_character_
        ),
      Edad_menor30          =
        case_when(
          Edad_menor30          == 1 ~ "Menor a 30 años",
          Edad_menor30          == 0 ~ "Mayor o igual a 30 años",
          T ~ NA_character_
        )
    )
  
  selectables <- c(selectables)
  
  logit_data <- master_data.df %>%
    select(all_of(selectables),
           all_of(dependent_var),
           Delito_unico_categ, 
           Estado_arresto) %>%
    mutate(
      Educacion_superior =
        if_else(
          Educacion_superior %in% "Cuenta con título de educación universitaria",
          "ZCuenta con título de educación universitaria", Educacion_superior
        ),
      Sexo                  =
        if_else(
          Sexo %in% "Femenino",
          "ZFemenino", Sexo
        ),
      Color_piel_claro       =
        if_else(
          Color_piel_claro %in% "Color de piel claro",
          "ZColor de piel claro", Color_piel_claro
        ),
      LGBTQ                 =
        if_else(
          LGBTQ %in% "Pertenece a la comunidad LGBTQ",
          "ZPertenece a la comunidad LGBTQ", LGBTQ
        ),
      Etnia                 =
        if_else(
          Etnia %in% "Afroamericano o indígena",
          "ZAfroamericano o indígena", Etnia
        ),
      Edad_menor30          =
        if_else(
          Edad_menor30 %in% "Menor a 30 años",
          "ZMenor a 30 años", Edad_menor30
        ),
    ) %>%
    arrange(Sexo, Educacion_superior, Color_piel_claro, LGBTQ, Etnia, Edad_menor30, 
            Delito_unico_categ, Estado_arresto)
  
  formula <- selectables %>%
    t() %>%
    as.data.frame() %>%
    unite(., formula, sep = "+") %>%
    as.character()
  
  depVar <- dependent_var
  
  formula  <- as.formula(paste(depVar, "~", 
                               formula, 
                               "+factor(Delito_unico_categ)+factor(Estado_arresto)")
  )
  logit    <- glm(formula,
                  data   = logit_data, 
                  family = "binomial")
  return(logit)
}


logit_dataBase_mg.fn <- function(data = Main_database,
                              selectables = c("Sexo", 
                                              "Educacion_superior", 
                                              "Color_piel_claro", 
                                              "LGBTQ", 
                                              "Etnia", 
                                              "Edad_menor30"),
                              dependent_var
) {
  
  master_data.df <- data %>%
    filter(Anio_arresto >= 2008) %>%
    filter(NSJP == 1) %>%
    filter(Delito_unico == 1) %>%
    mutate(
      Educacion_superior = 
        case_when(
          Educacion_superior == 1 ~ "Cuenta con título de educación universitaria",
          Educacion_superior == 0 ~ "No cuenta con título de educación universitario",
          T ~ NA_character_
        ),
      Color_piel_claro       =
        case_when(
          Color_piel_claro      == 1 ~ "Color de piel claro",
          Color_piel_claro      == 0 ~ "Color de piel oscuro",
          T ~ NA_character_
        ),
      LGBTQ                 = 
        case_when(
          LGBTQ                 == 1 ~ "Pertenece a la comunidad LGBTQ",
          LGBTQ                 == 0 ~ "No pertenece a la comunidad LGBTQ",
          T ~ NA_character_
        ),
      Etnia                 =
        case_when(
          Etnia                 == 1 ~ "Afroamericano o indígena",
          Etnia                 == 0 ~ "No se identifica con ninguna etnia",
          T ~ NA_character_
        ),
      Edad_menor30          =
        case_when(
          Edad_menor30          == 1 ~ "Menor a 30 años",
          Edad_menor30          == 0 ~ "Mayor o igual a 30 años",
          T ~ NA_character_
        )
    )
  
  selectables <- c(selectables)
  
  logit_data <- master_data.df %>%
    select(all_of(selectables),
           all_of(dependent_var),
           Delito_unico_categ, 
           Estado_arresto) %>%
    mutate(
      Educacion_superior =
        if_else(
          Educacion_superior %in% "Cuenta con título de educación universitaria",
          "ZCuenta con título de educación universitaria", Educacion_superior
        ),
      Sexo                  =
        if_else(
          Sexo %in% "Femenino",
          "ZFemenino", Sexo
        ),
      Color_piel_claro       =
        if_else(
          Color_piel_claro %in% "Color de piel claro",
          "ZColor de piel claro", Color_piel_claro
        ),
      LGBTQ                 =
        if_else(
          LGBTQ %in% "Pertenece a la comunidad LGBTQ",
          "ZPertenece a la comunidad LGBTQ", LGBTQ
        ),
      Etnia                 =
        if_else(
          Etnia %in% "Afroamericano o indígena",
          "ZAfroamericano o indígena", Etnia
        ),
      Edad_menor30          =
        if_else(
          Edad_menor30 %in% "Menor a 30 años",
          "ZMenor a 30 años", Edad_menor30
        ),
    ) %>%
    arrange(Sexo, Educacion_superior, Color_piel_claro, LGBTQ, Etnia, Edad_menor30, 
            Delito_unico_categ, Estado_arresto)
  
  formula <- selectables %>%
    t() %>%
    as.data.frame() %>%
    unite(., formula, sep = "+") %>%
    as.character()
  
  depVar <- dependent_var
  
  formula  <- as.formula(paste(depVar, "~", 
                               formula, 
                               "+factor(Delito_unico_categ)+factor(Estado_arresto)")
  )
  logit    <- glm(formula,
                  data   = logit_data, 
                  family = "binomial")
  
  summaryLogit <- bind_rows(
    as.data.frame(coef(logit))
  )
  
  margEff      <- as.data.frame(
    margins_summary(logit, data = logit$data)
  ) %>%
    filter(factor %in% c("SexoZFemenino", "LGBTQZPertenece a la comunidad LGBTQ", 
                         "Educacion_superiorZCuenta con título de educación universitaria", 
                         "Color_piel_claroZColor de piel claro", "EtniaZAfroamericano o indígena",
                         "Edad_menor30ZMenor a 30 años"))
  
  margEff$factor <-recode(margEff$factor,
                          "SexoZFemenino" = "Mujer",
                          "LGBTQZPertenece a la comunidad LGBTQ"                     = "Perteneciente a \ncomunidad LGBTQ",
                          "EtniaZAfroamericano o indígena"                           = "Afroamericano/a o indígena",
                          "Educacion_superiorZCuenta con título de educación universitaria"  = "Con educación univeristaria \n o más",
                          "Edad_menor30ZMenor a 30 años"                             = "Menor a 30 años",
                          "Color_piel_claroZColor de piel claro"                     = "Color de piel claro"
  )
  
  data2table <- margEff %>%
    mutate(order_variable =
             case_when(
               factor == "Mujer"                              ~ 1,
               factor == "Perteneciente a \ncomunidad LGBTQ"  ~ 2,
               factor == "Menor a 30 años"                    ~ 3,
               factor == "Con educación univeristaria \n o más" ~ 4,
               factor == "Afroamericano/a o indígena"         ~ 5,
               factor == "Color de piel claro"               ~ 6
             ),
           dependent_var  =
             dependent_var
    )
  
  return(data2table)
  
}



base_prueba <- Main_database %>%
  filter(Anio_arresto >= 2008) %>%
  filter(NSJP == 1) %>%
  filter(Delito_unico == 1)

ols_prueba<- lm(indi ~ Sexo + Educacion_superior + Color_piel_claro + LGBTQ + Etnia + Edad_menor30 + factor(Delito_unico_categ) + factor(Estado_arresto), data = base_prueba)
logit_prueba <- glm(PJ_3 ~ Sexo + Educacion_superior + Color_piel_claro + LGBTQ + Etnia + Edad_menor30 + factor(Delito_unico_categ) + factor(Estado_arresto), data = base_prueba, family="binomial")
logit_prueba_2 <- logit_dataBase.fn(data = Main_database, dependent_var = "PJ_3")
logit_prueba_3 <- logit_dataBase_mg.fn(data = Main_database, dependent_var = "PJ_3")



stargazer::stargazer(ols_prueba,logit_prueba, logit_prueba_2,type="text")



### NUEVA FUNCION



logit_dataBase.fn <- function(data = Main_database,
                              selectables = c("Sexo", 
                                              "Educacion_superior", 
                                              "Color_piel_claro", 
                                              "LGBTQ", 
                                              "Etnia", 
                                              "Edad_menor30", 
                                              "vulnerabilidad_economica",
                                              "discapacidad"),
                              dependent_var
) {
  
  master_data.df <- data %>%
    filter(Anio_arresto >= 2008) %>%
    filter(NSJP == 1) %>%
    filter(Delito_unico == 1) %>%
    mutate(
      Educacion_superior = 
        case_when(
          Educacion_superior == 1 ~ "Cuenta con título de educación universitaria",
          Educacion_superior == 0 ~ "No cuenta con título de educación universitario",
          T ~ NA_character_
        ),
      Color_piel_claro       =
        case_when(
          Color_piel_claro      == 1 ~ "Color de piel claro",
          Color_piel_claro      == 0 ~ "Color de piel oscuro",
          T ~ NA_character_
        ),
      LGBTQ                 = 
        case_when(
          LGBTQ                 == 1 ~ "Pertenece a la comunidad LGBTQ",
          LGBTQ                 == 0 ~ "No pertenece a la comunidad LGBTQ",
          T ~ NA_character_
        ),
      Etnia                 =
        case_when(
          Etnia                 == 1 ~ "Afromexicano o indígena",
          Etnia                 == 0 ~ "No se identifica con ninguna etnia",
          T ~ NA_character_
        ),
      Edad_menor30          =
        case_when(
          Edad_menor30          == 1 ~ "Menor a 30 años",
          Edad_menor30          == 0 ~ "Mayor o igual a 30 años",
          T ~ NA_character_
        ),
      vulnerabilidad_economica  =
        case_when(
          vulnerabilidad_economica == 1 ~ "Vulnerable economicamente",
          vulnerabilidad_economica == 0 ~ "No vulnerable economicamente",
          T ~ NA_character_
        ),
      discapacidad      =
        case_when(
          discapacidad == 1 ~ "Reporta algún tipo de discapacidad",
          discapacidad == 0 ~ "No presenta discapacidad",
          T ~ NA_character_
        )
    )
  
  selectables <- c(selectables)
  
  logit_data <- master_data.df %>%
    select(all_of(selectables),
           all_of(dependent_var),
           Delito_unico_categ, 
           Estado_arresto) %>%
    mutate(
      Educacion_superior =
        if_else(
          Educacion_superior %in% "Cuenta con título de educación universitaria",
          "ZCuenta con título de educación universitaria", Educacion_superior
        ),
      Sexo                  =
        if_else(
          Sexo %in% "Femenino",
          "ZFemenino", Sexo
        ),
      Color_piel_claro       =
        if_else(
          Color_piel_claro %in% "Color de piel claro",
          "ZColor de piel claro", Color_piel_claro
        ),
      LGBTQ                 =
        if_else(
          LGBTQ %in% "Pertenece a la comunidad LGBTQ",
          "ZPertenece a la comunidad LGBTQ", LGBTQ
        ),
      Etnia                 =
        if_else(
          Etnia %in% "Afromexicano o indígena",
          "ZAfromexicano o indígena", Etnia
        ),
      Edad_menor30          =
        if_else(
          Edad_menor30 %in% "Menor a 30 años",
          "ZMenor a 30 años", Edad_menor30
        ),
      vulnerabilidad_economica          =
        if_else(
          vulnerabilidad_economica %in% "Vulnerable economicamente",
          "ZVulnerable economicamente", Edad_menor30
        ),
      discapacidad          =
        if_else(
          discapacidad %in% "Reporta algún tipo de discapacidad",
          "ZReporta algún tipo de discapacidad", discapacidad
        ),
    ) %>%
    arrange(Sexo, Educacion_superior, Color_piel_claro, LGBTQ, Etnia, Edad_menor30, 
            Delito_unico_categ, Estado_arresto, vulnerabilidad_economica, discapacidad)
  
  formula <- selectables %>%
    t() %>%
    as.data.frame() %>%
    unite(., formula, sep = "+") %>%
    as.character()
  
  depVar <- dependent_var
  
  formula  <- as.formula(paste(depVar, "~", 
                               formula, 
                               "+factor(Delito_unico_categ)+factor(Estado_arresto)")
  )
  logit    <- glm(formula,
                  data   = logit_data, 
                  family = "binomial")
  
  summaryLogit <- bind_rows(
    as.data.frame(coef(logit))
  )
  
  margEff      <- as.data.frame(
    margins_summary(logit, data = logit$data)
  ) %>%
    filter(factor %in% c("SexoZFemenino", "LGBTQZPertenece a la comunidad LGBTQ", 
                         "Educacion_superiorZCuenta con título de educación universitaria", 
                         "Color_piel_claroZColor de piel claro", "EtniaZAfromexicano o indígena",
                         "Edad_menor30ZMenor a 30 años", "vulnerabilidad_economicaZVulnerable economicamente", 
                         "discapacidadZReporta algún tipo de discapacidad"))
  
  margEff$factor <-recode(margEff$factor,
                          "SexoZFemenino"                                            = "Mujer",
                          "LGBTQZPertenece a la comunidad LGBTQ"                     = "Perteneciente a \ncomunidad LGBTQ",
                          "EtniaZAfromexicano o indígena"                            = "Afromexicano/a o \nindígena",
                          "Educacion_superiorZCuenta con título de educación universitaria"  = "Con educación \nuniveristaria",
                          "Edad_menor30ZMenor a 30 años"                             = "Menor a 30 años",
                          "Color_piel_claroZColor de piel claro"                     = "Color de piel \nclaro",
                          "vulnerabilidad_economicaZVulnerable economicamente"       = "Vulnerable \neconómicamente",
                          "discapacidadZReporta algún tipo de discapacidad"          = "Persona con \ndiscapacidad"
  )
  
  data2table <- margEff %>%
    mutate(order_variable =
             case_when(
               factor == "Mujer"                                  ~ 1,
               factor == "Perteneciente a \ncomunidad LGBTQ"      ~ 2,
               factor == "Menor a 30 años"                        ~ 3,
               factor == "Con educación \nuniveristaria"          ~ 4,
               factor == "Afromexicano/a o \nindígena"            ~ 5,
               factor == "Color de piel \nclaro"                  ~ 6,
               factor == "Vulnerable \neconómicamente"            ~ 7,
               factor == "Persona con \ndiscapacidad"             ~ 8,
             ),
           dependent_var  =
             dependent_var
    )
  
  return(data2table)
  
}



#### LOGIT



logit_prueba_1 <- logit_dataBase.fn(data = Main_database, dependent_var = "PJ_1")
logit_prueba_2 <- logit_dataBase.fn(data = Main_database, dependent_var = "PJ_2")
logit_prueba_3 <- logit_dataBase.fn(data = Main_database, dependent_var = "PJ_3")
logit_prueba_4 <- logit_dataBase.fn(data = Main_database, dependent_var = "PJ_4")
logit_prueba_5 <- logit_dataBase.fn(data = Main_database, dependent_var = "PJ_5")
logit_prueba_6 <- logit_dataBase.fn(data = Main_database, dependent_var = "PJ_6")
logit_prueba_7 <- logit_dataBase.fn(data = Main_database, dependent_var = "PJ_7")
logit_prueba_8 <- logit_dataBase.fn(data = Main_database, dependent_var = "UAA_1")
logit_prueba_9 <- logit_dataBase.fn(data = Main_database, dependent_var = "UAA_2")
logit_prueba_10 <- logit_dataBase.fn(data = Main_database, dependent_var = "UAA_3")
logit_prueba_11 <- logit_dataBase.fn(data = Main_database, dependent_var = "UAA_4")
logit_prueba_12 <- logit_dataBase.fn(data = Main_database, dependent_var = "GDH_1")
logit_prueba_13 <- logit_dataBase.fn(data = Main_database, dependent_var = "GDH_2")

logits <- rbind(logit_prueba_1,logit_prueba_2,logit_prueba_3,logit_prueba_4,logit_prueba_5,logit_prueba_6,
                logit_prueba_7,logit_prueba_8,logit_prueba_9,logit_prueba_10,logit_prueba_11,logit_prueba_12,
                logit_prueba_13) 

logits2 <- rbind(logit_prueba_1,logit_prueba_2,logit_prueba_3,logit_prueba_4,logit_prueba_5,logit_prueba_6,
                logit_prueba_7,logit_prueba_8,logit_prueba_9,logit_prueba_10,logit_prueba_11,logit_prueba_12,
                logit_prueba_13) %>%
  select(factor, dependent_var, AME, lower, upper) %>%
  pivot_wider(names_from = dependent_var ,id_cols = factor, values_from = c("AME","lower","upper") )


#timeline

Main_database_2011 <- Main_database %>% filter(Anio_arresto == 2011, Delito_unico_categ != "amenazas")
Main_database_2012 <- Main_database %>% filter(Anio_arresto == 2012, Delito_unico_categ != "amenazas")
Main_database_2013 <- Main_database %>% filter(Anio_arresto == 2013, Delito_unico_categ != "amenazas")
Main_database_2014 <- Main_database %>% filter(Anio_arresto == 2014, Delito_unico_categ != "amenazas")
Main_database_2015 <- Main_database %>% filter(Anio_arresto == 2015, Delito_unico_categ != "amenazas")
Main_database_2016 <- Main_database %>% filter(Anio_arresto == 2016, Delito_unico_categ != "amenazas")
Main_database_2017 <- Main_database %>% filter(Anio_arresto == 2017, Delito_unico_categ != "amenazas")
Main_database_2018 <- Main_database %>% filter(Anio_arresto == 2018, Delito_unico_categ != "amenazas")
Main_database_2019 <- Main_database %>% filter(Anio_arresto == 2019, Delito_unico_categ != "amenazas")
Main_database_2020 <- Main_database %>% filter(Anio_arresto == 2020, Delito_unico_categ != "amenazas")
Main_database_2021 <- Main_database %>% filter(Anio_arresto == 2021, Delito_unico_categ != "amenazas")

#logit_timeline_2011 <- logit_dataBase.fn(data = Main_database_2011, dependent_var = "indicator_general_maxlimit") %>% mutate(anio = 2011)
#logit_timeline_2012 <- logit_dataBase.fn(data = Main_database_2012, dependent_var = "indicator_general_maxlimit") %>% mutate(anio = 2012)
#logit_timeline_2013 <- logit_dataBase.fn(data = Main_database_2013, dependent_var = "indicator_general_maxlimit") %>% mutate(anio = 2013)
logit_timeline_2014 <- logit_dataBase.fn(data = Main_database_2014, dependent_var = "indicator_general_maxlimit") %>% mutate(anio = 2014)
#logit_timeline_2015 <- logit_dataBase.fn(data = Main_database_2015, dependent_var = "indicator_general_maxlimit") %>% mutate(anio = 2015)
logit_timeline_2016 <- logit_dataBase.fn(data = Main_database_2016, dependent_var = "indicator_general_maxlimit") %>% mutate(anio = 2016)
logit_timeline_2017 <- logit_dataBase.fn(data = Main_database_2017, dependent_var = "indicator_general_maxlimit") %>% mutate(anio = 2017)
logit_timeline_2018 <- logit_dataBase.fn(data = Main_database_2018, dependent_var = "indicator_general_maxlimit") %>% mutate(anio = 2018)
logit_timeline_2019 <- logit_dataBase.fn(data = Main_database_2019, dependent_var = "indicator_general_maxlimit") %>% mutate(anio = 2019)
logit_timeline_2020 <- logit_dataBase.fn(data = Main_database_2020, dependent_var = "indicator_general_maxlimit") %>% mutate(anio = 2020)
logit_timeline_2021 <- logit_dataBase.fn(data = Main_database_2021, dependent_var = "indicator_general_maxlimit") %>% mutate(anio = 2021)


#logit_timeline2_2011 <- logit_dataBase.fn(data = Main_database_2011, dependent_var = "indicator_general_minlimit") %>% mutate(anio = 2011)
#logit_timeline2_2012 <- logit_dataBase.fn(data = Main_database_2012, dependent_var = "indicator_general_minlimit") %>% mutate(anio = 2012)
#logit_timeline2_2013 <- logit_dataBase.fn(data = Main_database_2013, dependent_var = "indicator_general_minlimit") %>% mutate(anio = 2013)
logit_timeline2_2014 <- logit_dataBase.fn(data = Main_database_2014, dependent_var = "indicator_general_minlimit") %>% mutate(anio = 2014)
#logit_timeline2_2015 <- logit_dataBase.fn(data = Main_database_2015, dependent_var = "indicator_general_minlimit") %>% mutate(anio = 2015)
logit_timeline2_2016 <- logit_dataBase.fn(data = Main_database_2016, dependent_var = "indicator_general_minlimit") %>% mutate(anio = 2016)
logit_timeline2_2017 <- logit_dataBase.fn(data = Main_database_2017, dependent_var = "indicator_general_minlimit") %>% mutate(anio = 2017)
logit_timeline2_2018 <- logit_dataBase.fn(data = Main_database_2018, dependent_var = "indicator_general_minlimit") %>% mutate(anio = 2018)
logit_timeline2_2019 <- logit_dataBase.fn(data = Main_database_2019, dependent_var = "indicator_general_minlimit") %>% mutate(anio = 2019)
logit_timeline2_2020 <- logit_dataBase.fn(data = Main_database_2020, dependent_var = "indicator_general_minlimit") %>% mutate(anio = 2020)
logit_timeline2_2021 <- logit_dataBase.fn(data = Main_database_2021, dependent_var = "indicator_general_minlimit") %>% mutate(anio = 2021)


logits3 <- rbind(logit_timeline_2014,
                logit_timeline_2016, logit_timeline_2017,logit_timeline_2018,logit_timeline_2019,logit_timeline_2020,
                logit_timeline_2021) %>%
  select(factor, dependent_var, AME, lower, upper, anio) %>%
  pivot_wider(names_from = anio ,id_cols = factor, values_from = c("AME","lower","upper"))


logits4 <- rbind(logit_timeline2_2014,
                 logit_timeline2_2016, logit_timeline2_2017,logit_timeline2_2018,logit_timeline2_2019,logit_timeline2_2020,
                 logit_timeline2_2021) %>%
  select(factor, dependent_var, AME, lower, upper, anio) %>%
  pivot_wider(names_from = anio ,id_cols = factor, values_from = c("AME","lower","upper"))

