# ----------------------------------------------------------------------------------------------------
# WRAPPER
# ----------------------------------------------------------------------------------------------------
# written
# Gabriel LEMYRE
# ----------------------------------------------------------------------------------------------------
# First version : 24 avril 2020
# Last version  : 24 avril 2020
# ----------------------------------------------------------------------------------------------------

# --------------------------------------------------------
# WRAPPER FUNCTION
# --------------------------------------------------------
# Routine d'importation de toutes les fonctions et
#   packages necessaires à l'entrainement, aux analyses
#   et aux tests sur les modèles
# --------------------------------------------------------

# --------------------------------------------------------
# Modification à l'environnement global
# --------------------------------------------------------
options(max.print=1000000)

# ————————————————————————————————————————————————————————————————————————————————————
# ////////////////////////////////////////////////////////////////////
# LIBRAIRIES INTERNES
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# -———————————————————————————————————————————————————————————————————————————————————

# --------------------------------------------------------
# Sourcing private libraries
# R-Tools
# --------------------------------------------------------
path.tools <- '~/Documents/GitHub/R-Tools'
setwd(path.expand(path.tools)) # Setting Sourcing path

# Essai d'importer les fonctions de base, 
#   necessaires pour obtenir la fonction "sources_correctly"
file <- "BasicFunctions.R"
fn <- try(source(file), silent = TRUE)
if (inherits(fn, "try-error")){
  stop(paste("\n-------------------------\nFile :",file,"\nError :",fn))
}

# Autres fonctions externes standards
sources_correctly("RunTime.R")
# --------------------------------------------------------

# --------------------------------------------------------
# Changement du document de travail
# --------------------------------------------------------
setwd(path.expand(path.code)) # Setting Sourcing path
sources_correctly("packages.R")


# --------------------------------------------------------
# Sourcing private libraries
# Fonctions de bases pour l'entrainement du modèle
# --------------------------------------------------------
# Obtention de la liste de toutes les fonctions .R proches de wrapper.R
source.files.liste <- file.list(path.code,
                                ignored.files.vector=c("packages.R","wrapper.R","chart_generator.R"),
                                print.inside.message = FALSE,
                                type="R",
                                from.git=FALSE)

source.files.liste.string <- paste(source.files.liste,sep="\n",collapse="")
file <- paste(path.code,"/file_list.txt",sep="")
file.test(file)
writeLines(source.files.liste, file)
# --------------------------------------------------------


# Obtention de la liste de toutes les fonctions .R proches de wrapper.R
# print('Before')
# file.names <- c(as.vector(read.table(file,
#                                      colClasses = "character"))[,1])
# 
# source.files.liste <- file.list(path.git,
#                                 ignored.files.vector=c("packages.R","wrapper.R","chart_generator.R"),
#                                 print.inside.message = FALSE,
#                                 type="R",
#                                 from.git=TRUE,
#                                 file.names=file.names)

n.files <- length(source.files.liste)

# Sourcing all files in the given list
hline()
cat("IMPORTATION DU MATERIEL INFORMATIQUE\n")
hline()
for (i in 1:n.files){
  cat("sourcing",source.files.liste[i],"\n")
  sources_correctly(source.files.liste[i])
}
cat(hline(),"\n")

# --------------------------------------------------------