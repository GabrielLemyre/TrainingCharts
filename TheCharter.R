# ----------------------------------------------------------------------------------------------------
# Making LaTeX Table envir. compatible code from R dataframes
# ----------------------------------------------------------------------------------------------------
# written
# Gabriel LEMYRE
# ----------------------------------------------------------------------------------------------------
# First version : 20 avril 2020
# ----------------------------------------------------------------------------------------------------
# Last version : 24 avril 2020
# ----------------------------------------------------------------------------------------------------
rm(list=ls())
username <- "GL"
nb.days <- 5
path.output <- '~/Documents/GitHub/TrainingCharts/code/latex'

file.git <- ""
script <- getURL(file.git, ssl.verifypeer = FALSE)


# --------------------------------------------------------
# Changement du document de travail
# --------------------------------------------------------
path.code <- '~/Documents/GitHub/TrainingCharts/code/R'
setwd(path.expand(path.code)) # Setting Sourcing path


# --------------------------------------------------------
# Wrapping of all sources in wd
# --------------------------------------------------------
source("wrapper.R")

nbrows <- 3

path.images <- '~/Documents/GitHub/TrainingCharts'
setwd(path.expand(path.images)) # Setting Sourcing path

# ——————————————————————————————————————————————————————————————————————————
# Obtention de la liste des images
# ——————————————————————————————————————————————————————————————————————————
liste.images <- Full.Source(paste(path.images,"/images", sep=""),ignored.files.vector=NULL,print.inside.message = FALSE, type="pdf")

n.images <- length(liste.images)

name.broken <- name_break(liste.images,n.images)
liste.noms <- name.broken$liste.noms
liste.img.finale <- name.broken$liste.img.finale

slash <- chr(92)

# Selection des dates
dates <- c(as.Date(today()) + 0:nb.days)
n.dates <- length(dates)

Full.chart <- preambule(n.images=n.images,
                        n.dates=n.dates,
                        slash=slash)

Full.chart <- paste(Full.chart,
                    gen.sport(strsplit(as.character(liste.img.finale$V2[1]),
                                       ".", 
                                       fixed = TRUE)[[1]][1],
                              liste.noms[1],
                              dates=dates,
                              nbrows=nbrows,
                              slash=slash),
                    sep="")

for (i in 2:n.images){
  if(i==n.images){total.line=TRUE}else{total.line <- FALSE}
  Full.chart <- paste(
    Full.chart,
    gen.sport(
      image = strsplit(as.character(liste.img.finale$V2[i]), ".", fixed = TRUE)[[1]][1],
      titre = liste.noms[i], 
      len = n.dates,
      nbrows=nbrows,
      slash=slash,
      total.line=total.line
    ),
    sep="",
    collapse=""
  )
}

Full.chart <- paste(Full.chart,slash,"end{document}",sep="")

Copie.Presse.Papier(Full.chart)
# print(Full.chart.tex, file = paste(path.output,"filename.tex",sep="/"), compress = FALSE) # https://unix.stackexchange.com/a/368184/16920

file <- paste(path.output,"/",username,"_chart.tex",sep="")
file.test(file)
writeLines(Full.chart, file)

