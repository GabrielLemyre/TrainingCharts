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

n <- length(liste.images)
liste.noms <- matrix(NA,ncol=n)

mat.ord.liste.images <- matrix(NA,nrow=n,ncol=2)

for (i in 1:n){
  # Obtention du nom du fichier
  str.split <- strsplit(liste.images[i], "/", fixed = TRUE)[[1]]
  n.temp <- length(str.split)
  file.name <- str.split[n.temp]
  print(file.name)
  
  # Obtention de son numero dans l'ordre
  split <- strsplit(strsplit(file.name,".",fixed=TRUE)[[1]][1], "_", fixed = TRUE)[[1]]
  number.split <- split[1]
  
  liste.noms[i] <- paste(split[1],paste(split[-1],sep=" ",collapse=" "),sep=". ",collapse=" ")
  
  mat.ord.liste.images[i,] <- c(as.numeric(number.split),liste.images[i])
}
ord.list.img.df <- as.data.frame(mat.ord.liste.images)
liste.img.finale <- ord.list.img.df[order(as.numeric(as.character(ord.list.img.df$V1))),]
liste.noms <- liste.noms[order(as.numeric(as.character(ord.list.img.df$V1)))]


# Selection des dates
dates <- c(as.Date(today()) + 0:60)

Full.chart <- gen.sport(strsplit(as.character(liste.img.finale$V2[1]), ".", fixed = TRUE)[[1]][1],liste.noms[1],dates=dates)

for (i in 2:n){
  Full.chart <- paste(Full.chart,gen.sport(strsplit(as.character(liste.img.finale$V2[i]), ".", fixed = TRUE)[[1]][1],liste.noms[i],dates=dates, len = length(dates)),sep="",collapse="")
}

Copie.Presse.Papier(Full.chart)



