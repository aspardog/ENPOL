# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Settings Presentations
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    Cristina Álvarez Venzor     (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     January 22th, 2024
##
## This version:      January 22th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Required packages                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required packages

if ('pacman' %in% rownames(installed.packages()) == FALSE) {
  install.packages(pacman,
                   dependencies = TRUE)
}
library(pacman)

p_load(char = c(
  
  # Visualizations
  "showtext", "ggtext", "ggsankey", "ggwaffle", "ggplotify", "gridExtra", "patchwork", "ggh4x", "ggrepel",
  
  # Data Loading
  "haven", "foreign", "openxlsx", "readxl", "writexl", "xlsx",
  
  # Utilities
  "margins", "english", "quarto", "kableExtra", "sysfonts", "magrittr",
  
  # Good 'ol Tidyverse
  "tidyverse"
  
))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Dropbox Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# OnDrive/SharePoint path

# You can find out your username by inputting Sys.info()["user"] on the R Console.

if (Sys.info()["user"] == "marcelo") {
  path2SP <- paste0("/Users/marcelo/Library/CloudStorage/OneDrive-SharedLibraries-WorldJusticeProject/Mexico - ENPOL/Data/")
  
} else if (Sys.info()["user"] == "santiagopardo") {
  path2SP <- paste0("/Users/santiagopardo/OneDrive - World Justice Project/ENPOL/Data")
  
} else if (Sys.info()["user"] == "USER VERO"){
  path2SP <- paste0("PATH VERO")
  
} else if (Sys.info()["user"] == "arturoluna"){
  path2SP <- paste0("/Users/arturoluna/Dropbox/ENPOL/")
  
} else if (Sys.info()["user"] == "cristinaalvarez"){
  path2SP <- paste0("/Users/cristinaalvarez/Library/CloudStorage/OneDrive-WorldJusticeProject/ENPOL/Data/")
  
} else{
  path2SP <- "PLEASE INSERT YOUR PERSONAL PATH TO THE  WJP - DATA ANALYTICS DIRECTORY"
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Fonts                                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Loading fonts
path2fonts <- paste0(path2SP, "6. Country Reports/0. Fonts/")
font_add(family     = "Lato Full",
         regular    = paste0(path2fonts, "Lato-Regular.ttf"),
         italic     = paste0(path2fonts, "Lato-LightItalic.ttf"),
         bold       = paste0(path2fonts, "Lato-Bold.ttf"),
         bolditalic = paste0(path2fonts, "Lato-BoldItalic.ttf"))
font_add(family  = "Lato Light",
         regular = paste0(path2fonts, "Lato-Light.ttf"))
font_add(family  = "Lato Black",
         regular = paste0(path2fonts, "Lato-Black.ttf"))
font_add(family  = "Lato Black Italic",
         regular = paste0(path2fonts, "Lato-BlackItalic.ttf"))
font_add(family  = "Lato Medium",
         regular = paste0(path2fonts, "Lato-Medium.ttf"))
showtext_auto()


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  WJP theme                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining a ggplot WJP theme
WJP_theme <- function() {
  theme(panel.background   = element_blank(),
        plot.background    = element_blank(),
        panel.grid.major   = element_line(size     = 0.25,
                                          colour   = "#5e5c5a",
                                          linetype = "dashed"),
        panel.grid.minor   = element_blank(),
        axis.title.y       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0)),
        axis.title.x       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(10, 0, 0, 0)),
        axis.text.y        = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C"),
        axis.text.x = element_text(family = "Lato Full",
                                   face   = "plain",
                                   size   = 3.514598*.pt,
                                   color  = "#524F4C"),
        axis.ticks  = element_blank(),
        plot.margin  = unit(c(0, 0, 0, 0), "points")
  ) 
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5.  Color Palette                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

mainCOLOR      <- c("#2a2a9A")
countryPalette <- c("#2a2a94", "#a90099", "#3273ff", "#fa4d57", "#9d61f2", "#43a9a7", "#efa700", "#2c6d4f")
binPalette     <- c("#003b8a", "#fa4d57")
barsPalette    <- c("#2a2a9A", "#E2E2F7")
glinesPalette  <- c("#2a2a94", "#a90099", "#3273ff")
rosePalette    <- c("#20204a", "#12006b", "#2e2e95", "#4e43dd", "#756ef9", "#9c94ff", "#b1a6ff",
                    "#cfb3ff", "#e2a4ff", "#f2aadc", "#ffd7f5")



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 6. Convertir variables a numeric                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

clean_columns.fn <- function(data, columnas, numero_interes) {
  data %>%
    mutate(across(all_of(columnas), ~{
      # Convertir a numérico
      columna_numerica <- as.numeric(.)
      
      # Aplicar reglas de recodificación
      if (all(columna_numerica %in% c(1, 0, NA), na.rm = TRUE)) {
        return(columna_numerica)
      } else if (all(columna_numerica %in% c(1, 2, 8, 9, NA), na.rm = TRUE)) {
        return(case_when(
          columna_numerica == 1 ~ 1,
          columna_numerica == 2 ~ 0,
          columna_numerica == 8 | columna_numerica == 9 ~ NA_real_,
          TRUE ~ columna_numerica
        ))
      } else if (all(columna_numerica %in% c(1, 2, 3, 4, 8, 9, NA), na.rm = TRUE)) {
        return(case_when(
          columna_numerica == 1 ~ 1,
          columna_numerica == 2 ~ 1,
          columna_numerica == 3 ~ 0,
          columna_numerica == 4 ~ 0,
          columna_numerica == 8 | columna_numerica == 9 ~ NA_real_,
          TRUE ~ columna_numerica
        ))
      } else {
        return(case_when(
          columna_numerica == numero_interes ~ 1,
          !is.na(columna_numerica) ~ 0,
          TRUE ~ NA_real_
        ))
      }
    }))
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 7. Excel descriptivas cruzadas                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tabla_excel_fn <- function(dataset,
                           var_prop,
                           var1,
                           var2,
                           var3,
                           varfilter,
                           filtervalue,
                           carpeta,
                           seccion,
                           nombre,
                           Dato){
  
  if (is.na(varfilter) == T) {
    
    # Without filter variable
    
    if (is.na(var3) == T) {
      
      if (is.na(var2) == T) {
        
        if (is.na(var1) == T) {
          
          # No cross-cut (only works for var_prop variables that take a value of 0 and 1)
          
          df <-
            dataset %>% 
            mutate(vara = 1) %>%
            group_by(vara) %>%
            summarise(Prop = mean(.data[[var_prop]], na.rm = T)) %>%
            cbind(Dato,.) %>%
            select(-vara)
          
        } else {
          
          # 1 variable
          
          df <-
            dataset %>% 
            group_by(.data[[var1]]) %>%
            summarise(Prop = mean(.data[[var_prop]], na.rm = T)) %>%
            pivot_wider(names_from = .data[[var1]], values_from = Prop) %>%
            cbind(Dato,.)
          if (var1 == "Sexo"){
            df <-  df %>% 
              mutate(Gap = df$Masculino - df$Femenino)}
          
        }
        
      } else {
        
        # 2 variables
        
        variables <- c(var1,var2)
        
        df<-
          dataset %>% 
          group_by(across(variables)) %>%
          summarise(Prop = mean(.data[[var_prop]], na.rm = T)) %>%
          pivot_wider(names_from = .data[[var1]], values_from = Prop) %>%
          cbind(Dato,.) %>% 
          arrange(by_group = .data[[var2]])
        
      } 
      
    } else {
      
      # 3 variables
      
      variables <- c(var2,var3)
      
      groups_i = subset %>% pull(.data[[var1]]) %>% unique() %>% sort()
      
      df = tibble()
      
      for(i in groups_i) {
        
        df_i <-
          dataset %>% 
          filter(.data[[var1]] == i) %>%
          group_by(across(variables)) %>%
          summarise(Prop = mean(.data[[var_prop]], na.rm = T)) %>%
          mutate('{var1}' := i) %>%
          arrange(.data[[var2]],.data[[var3]]) %>%
          pivot_wider(names_from = .data[[var2]], values_from = Prop)
        
        if (nrow(df_i) == 0){
          df_i = tibble()
        } else{
          df_i %<>% 
            cbind(Dato,.) %>% 
            arrange(by_group = .data[[var3]]) 
        }
        
        df = bind_rows(df,df_i) %>%
          select(Dato, .data[[var1]], .data[[var3]], sort(colnames(.)))
        
      }
    }
    
  } else {
    
    # With filter variable
    
    if (is.na(var3) == T) {
      
      if (is.na(var2) == T) {
        
        if (is.na(var1) == T) {
          
          # No cross-cut  (only works for var_prop variables that take a value of 0 and 1)
          
          df <-
            dataset %>% 
            filter(.data[[varfilter]] == filtervalue) %>%
            mutate(vara = 1) %>%
            group_by(vara) %>%
            summarise(Prop = mean(.data[[var_prop]], na.rm = T)) %>%
            cbind(Dato,.)  %>%
            select(-vara)
          
        } else {
          
          # 1 variable
          
          df <-
            dataset %>% 
            filter(.data[[varfilter]]  == filtervalue) %>%
            group_by(.data[[var1]]) %>%
            summarise(Prop = mean(.data[[var_prop]], na.rm = T)) %>%
            pivot_wider(names_from = .data[[var1]], values_from = Prop) %>%
            cbind(Dato,.)
          if (var1 == "Sexo"){
            df <-  df %>% 
              mutate(Gap = df$Masculino - df$Femenino)}
        }
        
      } else {
        
        # 2 variables
        
        variables <- c(var1,var2)
        
        df<-
          dataset %>% 
          filter(.data[[varfilter]]  == filtervalue) %>%
          group_by(across(variables)) %>%
          summarise(Prop = mean(.data[[var_prop]], na.rm = T)) %>%
          pivot_wider(names_from = .data[[var1]], values_from = Prop) %>%
          cbind(Dato,.) %>% 
          arrange(by_group = .data[[var2]])
      } 
      
    } else {
      
      # 3 variables
      
      variables <- c(var2,var3)
      
      groups_i = subset %>% pull(.data[[var1]]) %>% unique() %>% sort()
      
      df = tibble()
      
      for(i in groups_i){
        
        df_i <-
          dataset  %>% 
          filter(.data[[varfilter]]  == filtervalue) %>%
          filter(.data[[var1]] == i) %>%
          group_by(across(variables)) %>%
          summarise(Prop = mean(.data[[var_prop]], na.rm = T)) %>%
          mutate('{var1}' := i) %>%
          arrange(.data[[var2]],.data[[var3]]) %>%
          pivot_wider(names_from = .data[[var2]], values_from = Prop) 
        
        
        if (nrow(df_i)==0){
          df_i = tibble()
        } else{
          df_i %<>% 
            cbind(Dato,.) %>% 
            arrange(by_group = .data[[var3]]) 
        }
        
        df = bind_rows(df,df_i) %>%
          select(Dato, .data[[var1]], .data[[var3]], sort(colnames(.)))
        
        
      }
      
    }
  }
  
  write.xlsx(as.data.frame(df), 
             file      = file.path(paste0(path2SP,
                                          "/National/Exploration/Input/Debido_proceso/", 
                                          carpeta,"/desc_", seccion,".xlsx"),
                                   fsep = "/"),  
             sheetName = paste0(nombre),
             append    = T,
             row.names = F)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 7.  Set data to plot                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set_data.fn <- function(data, columnas, labels) {
  porcentajes <- sapply(data[, columnas], function(columna) {
    sum(columna == 1, na.rm = TRUE) / sum(!is.na(columna))
  })
  
  # Calculate the count of non-NA observations per column
  counts <- sapply(data[, columnas], function(columna) {
    sum(!is.na(columna))
  })
  
  data2plot <- data.frame(
    Columna = names(porcentajes),
    PorcentajeUnos = round((porcentajes * 100), 1),
    Observaciones = counts, # Include counts here
    label = labels
  )
  
  data2plot <- data2plot %>%
    mutate(
      order_var = rank(-PorcentajeUnos, ties.method = "min"), 
      labels = str_wrap(label, width = 30), # Make sure the column name matches what's in the dataframe
      figure = paste0(PorcentajeUnos, "%")
    )
  
  return(data2plot)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 11.  Set data to plot  grid multiple bars                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set_data_multiple.fn <- function(data, columnas, labels, category) {
  porcentajes <- sapply(data[, columnas], function(columna) {
    sum(columna == 1, na.rm = TRUE) / sum(!is.na(columna))
  })
  
  # Calculate the count of non-NA observations per column
  counts <- sapply(data[, columnas], function(columna) {
    sum(!is.na(columna))
  })
  
  data2plot <- data.frame(
    Columna = names(porcentajes),
    PorcentajeUnos = round((porcentajes * 100), 1),
    Observaciones = counts, # Include counts here
    label = labels,
    category = category
  )
  
  data2plot <- data2plot %>%
    mutate(
      order_var = rank(-PorcentajeUnos, ties.method = "min"), 
      labels = str_wrap(label, width = 30), # Make sure the column name matches what's in the dataframe
      figure = paste0(PorcentajeUnos, "%")
    )
  
  return(data2plot)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 8.  Bar graph Ones Percentage                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

BarSimpleChartViz <- function(data = data2plot, 
                              x_var = category, 
                              y_var = value2plot, 
                              label_var = figure, 
                              fill_var = category, 
                              Observaciones = Observaciones,
                              fill_colors = fill_colors,
                              order_var = order_var,
                              title = title) {
  plt <- ggplot(data, 
                aes(x     = reorder({{x_var}},{{order_var}}),
                    y     = {{y_var}},
                    label = paste0({{label_var}}, "\n", "N =", {{Observaciones}}),
                    fill  = {{fill_var}})) +
    geom_bar(stat = "identity",
             show.legend = FALSE, width = 0.9)+
    scale_fill_manual(values = {{fill_colors}}) +
    geom_text(aes(y    = {{y_var}} + 5),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    labs(y = "% of respondents",
         title = title) +
    scale_y_continuous(limits = c(0, 105),
                       breaks = seq(0, 100, 20),
                       labels = paste0(seq(0, 100, 20), "%"),
                       position = "right") +
    scale_x_discrete(limits = rev) +
    coord_flip() +
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D0D1D3"),
          axis.title.y       = element_blank(),
          axis.title.x       = element_blank(),
          axis.text.y        = element_text(hjust = 1, size = 10),
          plot.title = element_text(face = "bold", size = 12))
  
  return(plt)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 9.  Bar graph Ones Percentage grid                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

BarSimpleChartViz_grid <- function(data = data2plot, 
                              x_var = category, 
                              y_var = value2plot, 
                              label_var = figure, 
                              fill_var = category, 
                              Observaciones = Observaciones,
                              fill_colors = fill_colors,
                              order_var = order_var,
                              title = title) {
  plt <- ggplot(data, 
                aes(x     = reorder({{x_var}},{{Observaciones}}),
                    y     = {{y_var}},
                    label = paste0({{label_var}}, "\n", "N =", {{Observaciones}}),
                    fill  = {{fill_var}})) +
    geom_bar(stat = "identity",
             show.legend = FALSE, width = 0.9)+
    scale_fill_manual(values = {{fill_colors}}) +
    geom_text(aes(y    = {{y_var}} + 5),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    labs(y = "% of respondents",
         title = title) +
    scale_y_continuous(limits = c(0, 105),
                       breaks = seq(0, 100, 20),
                       labels = paste0(seq(0, 100, 20), "%"),
                       position = "right") +
    scale_x_discrete(limits = rev) +
    coord_flip() +
    facet_grid(rows = vars(category), scales = "free_y", switch = "y", space = "free_y") +
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D0D1D3"),
          axis.title.y       = element_blank(),
          axis.title.x       = element_blank(),
          axis.text.y        = element_text(hjust = 1, size = 10),
          plot.title = element_text(face = "bold", size = 12),
          strip.placement = "outside")
  
  return(plt)
}



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 8.  Bar graph Ones Percentage                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

valores <- function(data, variables) {
  map(variables, ~{
    cat("Valores únicos de", .x, ":\n")
    print(unique(data[[.x]]))
    cat("\n")
  })
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 9.  logit database                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

logit_dataBase.fn <- function(data = Main_database_2008,
                              selectables = c("Sexo", 
                                              "Educacion_obligatoria", 
                                              "Colo_piel_claro", 
                                              "LGBTQ", 
                                              "Etnia", 
                                              "Edad_menor30", 
                                              "Ingreso_inseguro"),
                              dependent_var
) {
  
  master_data.df <- Main_database_2008 %>%
    filter(Anio_arresto >= 2015) %>%
    filter(NSJP == 1) %>%
    filter(Delito_unico == 1) %>%
    mutate(
      Educacion_obligatoria = 
        case_when(
          Educacion_obligatoria == 1 ~ "Título de bachiller o más",
          Educacion_obligatoria == 0 ~ "No cuenta con título de bachiller"
        ),
      Colo_piel_claro       =
        case_when(
          Colo_piel_claro      == 1 ~ "Color de piel clara",
          Colo_piel_claro      == 0 ~ "Color de piel oscura",
        ),
      LGBTQ                 = 
        case_when(
          LGBTQ                 == 1 ~ "Pertenece a la comunidad LGBTQ",
          LGBTQ                 == 0 ~ "No pertenece a la comunidad LGBTQ"
        ),
      Etnia                 =
        case_when(
          Etnia                 == 1 ~ "Afroamericano o indígena",
          Etnia                 == 0 ~ "No se identifica con ninguna etnia"
        ),
      Ingreso_inseguro      =
        case_when(
          Ingreso_inseguro      == 1 ~ "Financieramente inseguro",
          Ingreso_inseguro      == 0 ~ "Financieramente seguro"
        ),
      Edad_menor30          =
        case_when(
          Edad_menor30          == 1 ~ "Menor a 30 años",
          Edad_menor30          == 0 ~ "Mayor a 30 años"
        )
    )
  
  selectables <- selectables
  
  logit_data <- master_data.df %>%
    select(all_of(selectables),
           all_of(dependent_var)) %>%
    mutate(
      Educacion_obligatoria =
        if_else(
          Educacion_obligatoria %in% "No cuenta con título de bachiller",
          "ZNo cuenta con título de bachiller", Educacion_obligatoria
        ),
      Sexo                  =
        if_else(
          Sexo %in% "Femenino",
          "ZFemenino", Sexo
        ),
      Colo_piel_claro       =
        if_else(
          Colo_piel_claro %in% "Color de piel oscura",
          "ZColor de piel oscura", Colo_piel_claro
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
      Ingreso_inseguro      =
        if_else(
          Ingreso_inseguro %in% "Financieramente inseguro",
          "ZFinancieramente inseguro", Ingreso_inseguro
        ),
      Edad_menor30          =
        if_else(
          Edad_menor30 %in% "Menor a 30 años",
          "ZMenor a 30 años", Edad_menor30
        ),
    ) %>%
    arrange(Sexo, Educacion_obligatoria, Colo_piel_claro, LGBTQ, Etnia, Edad_menor30, Ingreso_inseguro)
  
  formula <- selectables %>%
    t() %>%
    as.data.frame() %>%
    unite(., formula, sep = "+") %>%
    as.character()
  
  depVar <- dependent_var
  
  formula  <- as.formula(paste(depVar, "~", formula))
  logit    <- glm(formula,
                  data   = logit_data, 
                  family = "binomial")
  
  summaryLogit <- bind_rows(
    as.data.frame(coef(logit))
  )
  
  margEff      <- as.data.frame(
    margins_summary(logit, data = logit$data)
  )
  
  margEff$factor <-recode(margEff$factor,
                          "SexoZFemenino" = "Mujer",
                          "LGBTQZPertenece a la comunidad LGBTQ"                     = "Perteneciente a \ncomunidad LGBTQ",
                          "Ingreso_inseguroZFinancieramente inseguro"                = "Financieramente inseguro/a",
                          "EtniaZAfroamericano o indígena"                           = "Afroamericano/a o indígena",
                          "Educacion_obligatoriaZNo cuenta con título de bachiller"  = "Sin diploma bachiller",
                          "Edad_menor30ZMenor a 30 años"                             = "Menor a 30 años",
                          "Colo_piel_claroZColor de piel oscura"                     = "Color de piel oscura"
  )
  
  data2table <- margEff %>%
    mutate(order_variable =
             case_when(
               factor == "Mujer"                              ~ 1,
               factor == "Perteneciente a \ncomunidad LGBTQ"  ~ 2,
               factor == "Menor a 30 años"                    ~ 3,
               factor == "Sin diploma bachiller"              ~ 4,
               factor == "Financieramente inseguro/a"         ~ 5,
               factor == "Afroamericano/a o indígena"         ~ 6,
               factor == "Color de piel oscura"               ~ 7
             ),
           dependent_var  =
             dependent_var
    )
  
  return(data2table)
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 10.  Line Chart Data Base                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

event_study <- function(data.df = data_subset_tipo.df,
                        var_groups = "National",
                        var_analysis = c("flagrancia", 
                                         "orden_det", 
                                         "inspeccion", 
                                         "det_ninguna"),
                        section,
                        subsection,
                        name) {
  
  
  variables2analyze <- c(var_groups)
  
  variables2summarise <- c(var_analysis)
  
  master_data.df <- data.df %>%
    mutate(period = 
             case_when(
               years_since_NSJP < -1 & years_since_NSJP > -2 ~ "one_year_before",
               years_since_NSJP < -2 & years_since_NSJP > -3 ~ "two_years_before",
               years_since_NSJP < -3 & years_since_NSJP > -4 ~ "three_years_before",
               years_since_NSJP < -4 & years_since_NSJP > -5 ~ "four_years_before",
               years_since_NSJP < -5 & years_since_NSJP > -6 ~ "five_years_before",
               years_since_NSJP < -6 & years_since_NSJP > -7 ~ "six_years_before",
               years_since_NSJP < -7 & years_since_NSJP > -8 ~ "seven_years_before",
               years_since_NSJP < -8 & years_since_NSJP > -9 ~ "eight_years_before",
               years_since_NSJP < -9 & years_since_NSJP > -10 ~ "nine_years_before",
               years_since_NSJP < -10 & years_since_NSJP > -11 ~ "ten_years_before",
               years_since_NSJP > -1 & years_since_NSJP < 1  ~ "implementation_year",
               years_since_NSJP > 1 & years_since_NSJP < 2 ~ "one_year_after",
               years_since_NSJP > 2 & years_since_NSJP < 3 ~ "two_years_after",
               years_since_NSJP > 3 & years_since_NSJP < 4 ~ "three_years_after",
               years_since_NSJP > 4 & years_since_NSJP < 5 ~ "four_years_after",
               years_since_NSJP > 5 & years_since_NSJP < 6 ~ "five_years_after",
               years_since_NSJP > 6 & years_since_NSJP < 7 ~ "six_years_after",
               years_since_NSJP > 7 & years_since_NSJP < 8 ~ "seven_years_after",
               years_since_NSJP > 8 & years_since_NSJP < 9 ~ "eight_years_after",
               years_since_NSJP > 9 & years_since_NSJP < 10 ~ "nine_years_after",
               years_since_NSJP > 10 & years_since_NSJP < 11 ~ "ten_years_after",
               years_since_NSJP > 11 & years_since_NSJP < 12 ~ "eleven_years_after",
               years_since_NSJP > 12 & years_since_NSJP < 13 ~ "twelve_years_after",
               years_since_NSJP > 13 & years_since_NSJP < 14 ~ "thirteen_years_after",
               years_since_NSJP > 14 & years_since_NSJP < 15 ~ "fourteen_years_after"
             )) %>%
    filter(!is.na(period)) %>%
    arrange(years_since_NSJP) %>%
    mutate(National = "National")
  
  data2analysis <- lapply(variables2analyze, function(vars){
    
    data_subset.df <- master_data.df %>%
      mutate(var_name = as.character(vars)) %>%
      rename(group = all_of({{vars}}))
    
    changes_time <- data_subset.df %>% 
      group_by(period, group, var_name) %>%
      summarise(
        across(all_of(variables2summarise),
               ~ mean(.x, na.rm = TRUE))) %>%
      mutate(order_value = 
               case_when(
                 period == "ten_years_before"    ~ -10,
                 period == "nine_years_before"   ~ -9,
                 period == "eight_years_before"  ~ -8,
                 period == "seven_years_before"  ~ -7,
                 period == "six_years_before"    ~ -6,
                 period == "five_years_before"   ~ -5,
                 period == "four_years_before"   ~ -4,
                 period == "three_years_before"  ~ -3,
                 period == "two_years_before"    ~ -2,
                 period == "one_year_before"     ~ -1,
                 period == "implementation_year" ~ 0,
                 period == "one_year_after"      ~ 1,
                 period == "two_years_after"     ~ 2,
                 period == "three_years_after"   ~ 3,
                 period == "four_years_after"    ~ 4,
                 period == "five_years_after"    ~ 5,
                 period == "six_years_after"     ~ 6,
                 period == "seven_years_after"   ~ 7,
                 period == "eight_years_after"   ~ 8,
                 period == "nine_years_after"    ~ 9,
                 period == "ten_years_after"     ~ 10,
                 period == "eleven_years_after"  ~ 11,
                 period == "twelve_years_after"   ~ 12,
                 
               )
      ) %>%
      arrange(order_value)
    
  })
  
}



lineChartData.fn <-function(data = Main_database_2008,
                            dependent_var = dependent_var){
  
  dependent_var <- dependent_var
  
  event_study_tipo <- event_study(data.df = data, 
                                  var_analysis = dependent_var,
                                  var_groups = c("National"))
  
  data2table <- event_study_tipo[[1]]
  
  data2plot <- data2table %>%
    filter(order_value > -1) %>%
    rename(dependent_var = all_of(dependent_var)) %>%
    mutate(
      dependent_cmpl = 1 - dependent_var
    ) %>%
    drop_na() %>%
    pivot_longer(cols = c(dependent_var, dependent_cmpl), names_to = "category", values_to = "value2plot") %>%
    mutate(
      period = 
        case_when(
          period == "implementation_year" ~ "Implementación",
          period == "one_year_after" ~ "Un año",
          period == "two_years_after" ~ "Dos años",
          period == "three_years_after" ~ "Tres años",
          period == "four_years_after" ~ "Cuatro años",
          period == "five_years_after" ~ "Cinco años",
          period == "six_years_after" ~ "Seis años",
          period == "seven_years_after" ~ "Siete años",
          period == "eight_years_after" ~ "Ocho años",
          period == "nine_years_after" ~ "Nueve años",
          period == "ten_years_after" ~ "Diez años",
          period == "eleven_years_after" ~ "Once años",
          period == "twelve_years_after" ~ "Doce años",
        ),
      value2plot = value2plot*100,
      labels = if_else(
        period %in% c("Implementación", "Dos años", "Cuatro años", "Seis años", "Ocho años", "Diez años", "Doce años"),
        paste0(round(value2plot,0), "%"), NA_character_),
      period_labels =
        case_when(
          period == "Implementación" ~ "Implementación",
          period == "Un año" ~ " ",
          period == "Dos años" ~ "Dos años",
          period == "Tres años" ~ " ",
          period == "Cuatro años" ~ "Cuatro años",
          period == "Cinco años" ~ " ",
          period == "Seis años" ~ "Seis años",
          period == "Siete años" ~ " ",
          period == "Ocho años" ~ "Ocho años",
          period == "Nueve años" ~ " ",
          period == "Diez años" ~ "Diez años",
          period == "Once años" ~ " ",
          period == "Doce años" ~ "Doce años",
          
        )
    )
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Line Chart                                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

lineChartViz <- function(data = data2plot,
                         period = period,
                         order_value = order_value,
                         category = category,
                         labels = labels,
                         colors = category) {
  # Creating ggplot
  plt <- ggplot(data2plot, 
                aes(x     = reorder(period, order_value),
                    y     = value2plot,
                    color = category,
                    label = labels,
                    group = category)) +
    geom_point(size = 2,
               show.legend = F) +
    geom_line(size  = 1,
              show.legend = F) +
    geom_text_repel(family      = "Lato Full",
                    fontface    = "bold",
                    size        = 3.514598,
                    show.legend = F,
                    
                    # Additional options from ggrepel package:
                    min.segment.length = 1000,
                    seed               = 42,
                    box.padding        = 0.5,
                    direction          = "y",
                    force              = 5,
                    force_pull         = 1) +
    scale_y_continuous(limits = c(0, 105),
                       expand = c(0,0),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20), "%")) +
    scale_x_discrete("period",
                     labels = c("Implementación" = "Implementación",
                                "Un año" = " ",
                                "Dos años" = "Dos años",
                                "Tres años" = " ",
                                "Cuatro años" = "Cuatro años",
                                "Cinco años" = " ",
                                "Seis años" = "Seis años",
                                "Siete años" = " ",
                                "Ocho años" = "Ocho años",
                                "Nueve años" = " ",
                                "Diez años" = "Diez años",
                                "Once años" = " ",
                                "Doce años" = "Doce años")
    ) +
    scale_color_manual(values = colors4plot) +
    WJP_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#d1cfd1"),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          axis.line.x        = element_line(color    = "#d1cfd1"),
          axis.ticks.x       = element_line(color    = "#d1cfd1",
                                            linetype = "solid")
    )
  
}




## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 11.  Logit Chart                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

logit_demo_panel <- function(mainData = data2plot,
                             line_color = "#003b8a",
                             line_size  = 2,
                             point_color = "#003b8a",
                             point_size   = 4) {
  
  plot <- ggplot(mainData, aes(x = reorder(factor, -order_variable), y = AME)) +
    geom_hline(yintercept = 0, lty = 1, color = "#fa4d57", lwd = 1)  +
    geom_linerange(aes(x = reorder(factor, -order_variable),  ymin = lower, ymax = upper),
                   lwd = line_size, position = position_dodge(width = .7), 
                   stat = "identity", color = line_color)+
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = point_size, position = position_dodge(width = .7), color = point_color) +
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = 2, position = position_dodge(width = .7), color = "white") +
    labs(y = "Menos probable                               Más probable") +
    scale_y_continuous(limits = c(-0.25, 0.25),
                       breaks = seq(-0.25, 0.25, by = 0.125),
                       expand = expansion(mult = 0.025), position = "right",
                       labels = c("-25", "-12.5", "0", "+12.5", "+25"))+
    WJP_theme()+
    coord_flip() +
    theme(legend.position = "none",
          panel.background   = element_blank(),
          panel.grid.major.x = element_line(colour = "#d1cfd1", 
                                            size = 0.5, linetype = "dashed"),
          legend.title = element_blank(),
          axis.title.y       = element_blank(),
          axis.text.y        = element_text(family = "Lato Medium",
                                            size     = 3.514598*.pt,
                                            color    = "#4a4a49",
                                            hjust    = 0),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x  = element_blank(),
          panel.grid.minor.y = element_blank())
  
  return(plot)
}

