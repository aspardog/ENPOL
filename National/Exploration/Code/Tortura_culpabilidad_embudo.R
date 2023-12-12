

#torurado, no torturado

table(Main_database$tortura_generalizada, useNA = "always")

#Culpable
table(Main_database$tortura_generalizada, Main_database$culpabilidad, useNA = "always")

#Declaró culpable P4.6.4
tortura <- subset(Main_database, tortura_generalizada == 0)
table(tortura$culpabilidad, tortura$P4_6_4, useNA = "always")

tortura <- subset(Main_database, tortura_generalizada == 1)
table(tortura$culpabilidad, tortura$P4_6_4, useNA = "always")

#Declaró culpable tipo prueba
tipo <- subset(Main_database, tortura_generalizada == 0)
tipo <- subset(tipo, culpabilidad ==  0)
table(tipo$P4_6_4, tipo$P5_15_01, useNA = "always")

tipo <- subset(Main_database, tortura_generalizada == 1)
tipo <- subset(tipo, culpabilidad ==  0)
table(tipo$P4_6_4, tipo$P5_15_01, useNA = "always")