## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL 2023 - Index descriptives 
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    D. Cristina Álvarez Venzor     (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     January 11th, 2024
##
## This version:      January 11th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Load Settings                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("National/Data_cleaning/Code/settings.R")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 

labels <- read_xlsx((paste0(path2DB,"National/Exploration/Input/col_names_label.xlsx")))

result_df <- apply_labels(result_df,  
                              P3_14_5 = "¿el policía o autoridad  informó sobre sus derechos a guardar silencio ...?",
                              P4_1_03  = "¿le dijeron de qué le acusaban?",
                              P4_1_04 =  "¿el agente del Mp le explicó sus derechos como a guardar silencio ... ?",
                              P4_1_13 =  "¿la autoridad contactó al consulado de su país?",                                      
                              P4_1_14 =   "¿necesitaba un traductor por no hablar español?",                                      
                              P4_1_15 =   "¿tuvo el apoyo de un traductor?",                                     
                              P4_1_16 =   "¿tuvo el apoyo de alguna persona en el MP por no saber leer o escribir?",           
                              P4_6_2  = "En el MP ¿le leyeron o le dieron a leer su declaración",
                              P4_6A_2 = "Una vez que leyó o le dieron a leer su declaración… ¿entendió su declaración?",                                 
                              P5_17_1 =  "En general, ¿qué tan claro su abogado defensor durante las audiencias?", 
                              P5_17_2 =   "el juez al explicar por qué tomaba sus decisiones",                                                             
                              P5_17_3 =  "el Fiscal o MP al acusarlo",                                                                                    
                              P5_17_4 =   "la víctima o el abogado de la víctima",                                                                        
                              P5_20_4 =   "Durante las audiencias, ¿usted… podía escuchar lo que se decía? "
                              )

capacidad_legal <- c("P3_14_5","P4_1_03", "P4_1_04", "P4_1_13", "P4_1_14", "P4_1_15", "P4_1_16", "P4_6_2",
                                    "P4_6A_2", "P5_17_1", "P5_17_2", "P5_17_3", "P5_17_4",  "P5_20_4")
data <- Main_database[, capacidad_legal]
data$P3_14_5 <- as.character(data$P3_14_5)


convert_columns <- function(data, columns) {
  for (column in columns) {
    data <- data %>%
      mutate(!!sym(column) := case_when(
        !!sym(column) == "1" ~ 1,
        is.na(!!sym(column)) ~ NA,
        TRUE ~ 0
      ))
  }
  return(data)
}

data <- convert_columns(data, capacidad_legal)

calculate_percentage <- function(column) {
  total_non_na <- sum(!is.na(column))
  if (total_non_na > 0) {
    percentage_1 <- sum(column == 1, na.rm = TRUE) / total_non_na
    return(percentage_1 * 100)
  } else {
    return(NA)
  }
}

# Apply the function to each column and create a new dataframe
result_df <- data.frame(
  ColumnName = names(data),
  Per_Sí = sapply(data, calculate_percentage)
)

# View the result
print(result_df)


data <- merge(result_df, labels, by.x = "ColumnName", by.y= "col_name")
data <- data %>% arrange(Per_Sí)
data$Per_No <- 100 - data$Per_Sí 



datos <- data %>% pivot_longer(cols = c("Per_Sí", "Per_No"),
                      names_to = "answer",
                      #names_pattern = "(.+)_rate",
                      values_to = "percentage") 


ggplot(datos, aes(y = Lab, x = percentage, fill = answer)) +
  geom_bar(stat = "identity")

