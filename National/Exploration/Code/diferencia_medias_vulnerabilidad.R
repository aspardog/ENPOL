

Main_database <- Main_database %>% 
                      mutate(vulnerabilidad_economica = case_when(vulnerabilidad_economica == 0 ~ "NO",
                                                                  vulnerabilidad_economica == 1 ~ "SI",
                                                                  T ~ NA_character_)) %>% 
                      filter(complete.cases(vulnerabilidad_economica))


# Realizar la prueba t
t_test_result <- t.test(indicator_general ~ vulnerabilidad_economica, data = Main_database)

# Ver el resumen de los resultados
print(t_test_result)