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
setwd(path.expand(path.code)) # Setting Sourcing path

# path.git <- 'https://raw.githubusercontent.com/GabrielLemyre/TrainingCharts/master/code/R/'

# --------------------------------------------------------
# Wrapping of all sources in wd
# --------------------------------------------------------
source("wrapper.R")

setwd(path.expand(path.images)) # Setting Sourcing path

# ——————————————————————————————————————————————————————————————————————————
# Obtention de la liste des images
# ——————————————————————————————————————————————————————————————————————————
liste.images <- file.list(path.images,
                            ignored.files.vector=NULL,
                            print.inside.message = FALSE, 
                            type="pdf")


n.images <- length(liste.images)

name.broken <- name.break(liste.images,n.images)
liste.noms <- name.broken$liste.noms
liste.img.finale <- name.broken$liste.img.finale
liste.nb.rows <- name.broken$liste.nb.rows

n.rows <- 0
for (i in 1:n.images){ 
  # Par convension atuellement, les charts de 1 et 2 lignes, sont de 
  #   la même hauteur que celle de 3 lignes
  n.rows <- n.rows + switch(as.character(liste.nb.rows[i]),"1"=3,"2"=3,liste.nb.rows[i])
}

slash <- chr(92)

# Selection des dates
dates <- c(as.Date(today()) + 0:nb.days)
n.dates <- length(dates)

hline()
cat("CONSTRUCTION DES TABLEAUX\n")
hline()
# Construction du préambule du document LaTeX
Full.chart <- preambule(n.images=n.images,
                        n.dates=n.dates,
                        slash=slash,
                        image.width=image.width,
                        n.rows=n.rows)

# Ajout du premier sport, comprenant les dates
Full.chart <- paste(Full.chart,
                    gen.sport(strsplit(as.character(liste.img.finale$V2[1]),
                                       ".", 
                                       fixed = TRUE)[[1]][1],
                              liste.noms[1],
                              dates=dates,
                              nbrows=liste.nb.rows[1],
                              slash=slash,
                              image.width=image.width),
                    sep="")

# Ajout des n.images-1 images suivantes
total.line=FALSE
for (i in 2:n.images){
  if(i==n.images){
    total.line <- TRUE
  }else{
    total.line <- FALSE
  }
  Full.chart <- paste(
    Full.chart,
    gen.sport(
      image = strsplit(as.character(liste.img.finale$V2[i]), ".", fixed = TRUE)[[1]][1],
      titre = liste.noms[i], 
      len = n.dates,
      nbrows=liste.nb.rows[i],
      slash=slash,
      total.line=total.line,
      image.width=image.width
    ),
    sep="",
    collapse=""
  )
}

Full.chart <- paste(Full.chart,slash,"end{document}",sep="")
cat(hline(),"\n")

Copie.Presse.Papier(Full.chart)
# print(Full.chart.tex, file = paste(path.output,"filename.tex",sep="/"), compress = FALSE) # https://unix.stackexchange.com/a/368184/16920

file <- paste(path.output,"/",username,"_chart.tex",sep="")
file.test(file)
writeLines(Full.chart, file)

