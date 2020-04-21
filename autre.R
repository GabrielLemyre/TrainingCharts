# ——————————————————————————————————————————————————————————————————————————
# Copie le contenu d'une variable de type 'string' au presse-papier de l'usager
# ——————————————————————————————————————————————————————————————————————————
Copie.Presse.Papier <- function(string) {
  os <- Sys.info()[['sysname']]
  if (os == "Windows") { # Si systeme dexploitation windows
    return(utils::writeClipboard(string))
  } else if (os == "Darwin") { # Si systeme dexploitation iOS
    Mac.Copie.Presse.Papier <- function(string){
      presse.papier <- pipe("pbcopy", "w")
      cat(string, file = presse.papier, sep = "\n")
      close(presse.papier)	# Fermer lobjet presse-papier
    }
    return(Mac.Copie.Presse.Papier(string))
  }
}


# ——————————————————————————————————————————————————————————————————————————
# Transforme un nombre en son caractère associé
# ——————————————————————————————————————————————————————————————————————————
chr <- function(n) { rawToChar(as.raw(n)) }


# ——————————————————————————————————————————————————————————————————————————
# Listing all files in a directory and keeping a list of .R files with full paths
# ——————————————————————————————————————————————————————————————————————————
Full.Source <- function(path, first.pass=TRUE, ignored.files.vector=NULL, print.inside.message=FALSE){
  png.files <- c() # Initialisation de la liste de fonction .R
  
  # longueur de la liste des dossiers et fichiers à ignorer
  n.files.ignored <- length(ignored.files.vector)
  
  # Information sur le document analysé
  if (print.inside.message){ cat("\n# ------------------------------\n","Inside",path,"\n# ------------------------------\n")}
  
  # Obtention de la liste de tous les documents dans le fichier en path
  liste.document <- list.files(path, pattern=NULL, 
                               all.files=FALSE,
                               full.names=TRUE)
  
  # Impression de la liste des documents si c'est le premier appels de la foncion
  if (first.pass==TRUE){
    if (print.inside.message) print(liste.document);
  }
  
  for (i in 1:length(liste.document)){
    
    # Sinon on test pour les extensions ou lance la fonction dans les documents
    #   internes de l'appel actuel
    if (grepl("\\.png", liste.document[i])){ 
      # Si image png, ajout à la liste
      if (print.inside.message) cat("+",liste.document[i],"\n");
      png.files <- append(png.files,liste.document[i])
    } else if (grepl("\\.csv", liste.document[i])){ 
      # Si document csv, rien pour l'instant
      if (print.inside.message) cat("dataset",liste.document[i],"\n");
    } else if (!grepl("\\.", liste.document[i])){ 
      # Si aucune extension, on entre dans le document et roule la présente fonction
      if (print.inside.message) print("--> Going Deeper <--");
      png.files <- append(png.files,Full.Source(liste.document[i],first.pass=FALSE, ignored.files.vector, print.inside.message))
    } else { 
      # Si ce n'est pas un format recherché, on rejette et averti l'utilisateur
      if (print.inside.message) cat("REJECTING",liste.document[i],":",grep("\\.", liste.document[i]),"\n");
    }
    
  }
  
  return(png.files) # On retourne la liste à la fonction précédente
}