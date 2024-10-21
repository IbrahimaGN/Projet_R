library('dplyr')
#importer le jeu de données
Data<- read.table("/home/ibrahima/Documents/SONATEL ACADEMY/PYTHON/Projet_python_1/Donnees_Projet_Python_Dev_Data.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
View(Data) #Afficher le jeu de données

head(Data)	# affiche l'entête des données
#fonction pour valider les numeros
verifier_numero <- function(numero) {
  if (nchar(numero) == 7 && grepl("[A-Z]", numero) && grepl("[0-9]", numero)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
# Accéder à la deuxième colonne
colonne2 <- Data[[2]]
# Parcourir les éléments de la deuxième colonne avec une boucle for
for (element in colonne2) {
  numValid <- verifier_numero(element)
  print(paste("Numéro:", element, "| Valide:", numValid))
}


#fonction pour valider les prénoms
verifier_prenom <- function(prenom) {
  if (nchar(prenom) >= 3 && grepl("^[A-Za-z]", substr(prenom, 1, 1))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
# Accéder à la troisième colonne
colonne4 <- Data[[4]]
# Parcourir les éléments de la troisiéme colonne avec une boucle for
for (element in colonne4) {
  prenomValid <- verifier_prenom(element)
  print(paste("Prénom:", element, "| Valide:", prenomValid))
}


#fonction pour valider les noms
verifier_nom <- function(nom) {
  if (nchar(nom) >= 2 && grepl("^[A-Za-z]", substr(nom, 1, 1))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
# Accéder à la troisième colonne
colonne3 <- Data[[3]]

# Parcourir les éléments de la troisiéme colonne avec une boucle for
for (element in colonne3) {
  nomValid <- verifier_nom(element)
  print(paste("Nom:", element, "| Valide:", nomValid))
}

convertir_date <- function(date_naissance) {
  if (is.null(date_naissance)) {
    return(FALSE)
  }
  
  date_formats <- c('%d/%m/%Y', '%d-%m-%Y', '%d %m %Y', '%d,%m,%Y', '%d:%m:%Y', '%d/%m/%y', '%d-%m-%y', '%d %m %y', '%d,%m,%y', '%d:%m:%y',
                    '%d.%m.%y', '%d.%m.%Y', '%d|%m|%Y', '%d,%B,%y', '%d/%B/%y', '%d_%m_%Y', '%d_%m_%y')
  # Remplacer les caractères spéciaux par "/"
  date_naissance <- gsub("[_ -,.|:]", "/", date_naissance)
  
  for (fmt in date_formats) {
    try_date <- tryCatch({
      as.Date(date_naissance, format = fmt)
    }, error = function(e) {
      NA
    })
    if (!is.na(try_date)) {
      date_naissance <-format(try_date, "%d/%m/%y")
      return(TRUE)
    }
  }
  return(FALSE)
}

# Accéder à la troisième colonne
colonne5 <- Data[[5]]

# Parcourir les éléments de la troisiéme colonne avec une boucle for
for (element in colonne5) {
  date_valid <- convertir_date(element)
  print(paste("Date:", element, "| Valide:", date_valid))
}


#fonction pour convertir les classes
convertir_classe <- function(classe) {
  classe_str <- gsub("[ieèm]+", "e", classe, ignore.case=TRUE)
  classe_str <- gsub("\\s+", "", classe_str)
  classe_str <- paste0(tolower(substr(classe_str, 1, nchar(classe_str) - 1)), toupper(substr(classe_str, nchar(classe_str), nchar(classe_str))))
  return(classe_str)
}
#fonction pour valider les classes
valider_classe <- function(classe) {
  regex_classe <- "^[3-6]e[ABCD]$"
  classe_convertie <- convertir_classe(classe)
  if (grepl(regex_classe, classe_convertie, ignore.case=TRUE)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
# Accéder à la troisième colonne
colonne6 <- Data[[6]]
# Parcourir les éléments de la troisiéme colonne avec une boucle for
for (element in colonne6) {
  classeValid <- valider_classe(element)
  print(paste("Classe:", element, "| Valide:", classeValid))
}
validation <- function() {
  table_data1 <- list()
  table_data2 <- list()
  data <- read.csv("/home/ibrahima/Documents/SONATEL ACADEMY/PYTHON/Projet_python_1/Donnees_Projet_Python_Dev_Data.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
  
  for (i in 1:nrow(data)) {
    if (verifier_numero(data[i, "Numero"]) && verifier_nom(data[i, "Nom"]) &&
        verifier_prenom(data[i, "Prénom"]) && convertir_date(data[i, "Date.de.naissance"]) &&
        valider_classe(data[i, "Classe"])) {
      table_data1 <- rbind(table_data1, data[i, ])
    } else {
      table_data2 <- rbind(table_data2, data[i, ])
    }
  }
  
  return(list(table_data1, table_data2))
}



resultat1 <- validation()
table_data1 <- resultat[[1]]
print(table_data1)

resultat2 <- validation()
table_data2 <- resultat[[2]]
print(table_data2)

