# ----------------------------------------------------------------------------------------------------
# Making LaTeX Table envir. compatible code from R dataframes
# ----------------------------------------------------------------------------------------------------
# written
# Gabriel LEMYRE
# ----------------------------------------------------------------------------------------------------
# First version : lundi, 20 avril 2020
# ----------------------------------------------------------------------------------------------------
# Last version : lundi, 20 avril 2020
# ----------------------------------------------------------------------------------------------------


# --------------------------------------------------------
# Changement du document de travail
# --------------------------------------------------------
path <- '~/Documents/GitHub/TrainingCharts'
setwd(path.expand(path)) # Setting Sourcing path

# install.packages("lubridate")
library("lubridate")
# Essai d'importer le generateur de sport
file <- "sport_generator.R"
fn <- try(source(file), silent = TRUE)
if (inherits(fn, "try-error")){
  stop(paste("\n-------------------------\nFile :",file,"\nError :",fn))
}


# Essai d'importer les fonctions externes
file <- "autre.R"
fn <- try(source(file), silent = TRUE)
if (inherits(fn, "try-error")){
  stop(paste("\n-------------------------\nFile :",file,"\nError :",fn))
}

# ——————————————————————————————————————————————————————————————————————————
# Obtention de la liste des images
# ——————————————————————————————————————————————————————————————————————————
liste.images <- Full.Source(paste(path,"/images", sep=""),ignored.files.vector=c(),print.inside.message = FALSE)
liste.noms <- rep("Feuille 1",length(liste.images))

dates <- c(as.Date(today()) + 1:20)

Full.chart <- gen.sport(strsplit(liste.images[1], ".", fixed = TRUE)[[1]][1],"Feuille 1",dates=dates)

for (i in 2:length(liste.images)){
  Full.chart <- paste(Full.chart,gen.sport(strsplit(liste.images[i], ".", fixed = TRUE)[[1]][1],"Feuille 1",dates=dates, len = length(dates)),sep="",collapse="")
}

Copie.Presse.Papier(Full.chart)



