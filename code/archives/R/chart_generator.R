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

# Raccourcis pour les segments de texte nécessitant l'utilisation de chr()
slash <- chr(92)

# ——————————————————————————————————————————————————————————————————————————
# Obtention de la liste des images
# ——————————————————————————————————————————————————————————————————————————
liste.images <- file.list(path.images,
                            ignored.files.vector=NULL,
                            print.inside.message = FALSE, 
                            type="pdf")


n.images <- length(liste.images)

n.textes <- dim(liste.section.supp.texte)[1]
  
if (is.null(n.textes)){
  n.textes <- 0
}

n.section <- n.textes + n.images

# Sert à garder le compte en présence de text sans image
index.segment <- 0

name.broken <- name.break(liste.images)

# Ajout des sections de texte
liste.noms <- name.broken$liste.noms
liste.img.finale <- name.broken$liste.img.finale
liste.nb.rows <- name.broken$liste.nb.rows

n.rows <- 0
# Ajout ligne d'images
for (i in 1:n.images){ 
  # Par convention atuellement, les charts de 1 et 2 lignes, sont de 
  #   la même hauteur que celle de 3 lignes
  n.rows <- n.rows + switch(as.character(liste.nb.rows[i]),"1"=3,"2"=3,liste.nb.rows[i])
}
# Ajout ligne de textes
if(n.textes>0){
  for (i in 1:n.textes){ 
    # Par convention atuellement, les charts de 1 et 2 lignes, sont de 
    #   la même hauteur que celle de 3 lignes
    n.rows <- n.rows + max(as.numeric(liste.section.supp.texte[i,2]),3)
  }
}


# Selection des dates
if (!is.null(nb.days)){
  dates <- c(as.Date(today()) + -1:nb.days)
} else {
  dates <- seq(as.Date(De), as.Date(A), by="days")
}
n.dates <- length(dates)

hline()
cat("CONSTRUCTION DES TABLEAUX\n")
hline()

total.line=FALSE

# Construction du préambule du document LaTeX
Full.chart <- preambule(n.section=n.section,
                        n.dates=n.dates,
                        slash=slash,
                        image.width=image.width,
                        n.rows=n.rows,
                        cell.w.cm=cell.w.cm)

if(n.textes>0){
  is.text=T
  first.section <- liste.section.supp.texte[1,1]
  titre.premiere.section <- "Planification"
  nb.rows <- as.numeric(liste.section.supp.texte[1,2])
} else {
  is.text=F
  first.section <- strsplit(as.character(liste.img.finale$path[1]), ".", fixed = TRUE)[[1]][1]
  titre.premiere.section <- liste.noms[1]
  nb.rows <- liste.nb.rows[1]
}

# Ajout du premier sport, comprenant les dates
index.segment <- index.segment+1
Full.chart <- paste(Full.chart,
                    gen.sport(image=first.section,
                              titre=titre.premiere.section,
                              dates=dates,
                              nbrows=nb.rows,
                              slash=slash,
                              image.width=image.width,
                              is.text=is.text,
                              cell.w.cm=cell.w.cm,
                              numero.segment=index.segment),
                    sep="")

if(n.textes>1){
  for (i in 2:n.textes){
    index.segment <- index.segment+1
    Full.chart <- paste(
      Full.chart,
      gen.sport(
        image = liste.section.supp.texte[i,1],
        titre = "Planification", 
        longueur.date = n.dates,
        nbrows=as.numeric(liste.section.supp.texte[i,2]),
        slash=slash,
        total.line=total.line,
        image.width=image.width,
        is.text=T,
        cell.w.cm=cell.w.cm,
        numero.segment=index.segment
      ),
      sep="",
      collapse=""
    )
  }
}
  
# Ajout des n.images-1 images suivantes
if (n.images>1){
  if(n.textes>0){
    indice.debut <- 1
  } else {
    indice.debut <- 2
  }
  for (i in indice.debut:n.images){
    if(i==n.images){
      total.line <- TRUE
    }else{
      total.line <- FALSE
    }
    index.segment <- index.segment+1
    Full.chart <- paste(
      Full.chart,
      gen.sport(
        image = strsplit(as.character(liste.img.finale$path[i]), ".", fixed = TRUE)[[1]][1],
        titre = liste.noms[i], 
        longueur.date = n.dates,
        nbrows=liste.nb.rows[i],
        slash=slash,
        total.line=total.line,
        image.width=image.width,
        cell.w.cm=cell.w.cm,
        numero.segment = index.segment
      ),
      sep="",
      collapse=""
    )
  }
}


Full.chart <- paste(Full.chart,slash,"end{document}",sep="")
cat(hline(),"\n")

Copie.Presse.Papier(Full.chart)
# print(Full.chart.tex, file = paste(path.output,"filename.tex",sep="/"), compress = FALSE) # https://unix.stackexchange.com/a/368184/16920

file <- paste(path.output,"/",username,"_chart.tex",sep="")
file.test(file)
writeLines(Full.chart, file)

