# ----------------------------------------------------------------------------------------------------
# Creates the file preambule string
# ----------------------------------------------------------------------------------------------------
# written
# Gabriel LEMYRE
# ----------------------------------------------------------------------------------------------------
# First version : 25 avril 2020
# ----------------------------------------------------------------------------------------------------
# Last version : 25 avril 2020
# ----------------------------------------------------------------------------------------------------

preambule <- function(n.images,n.dates,slash){
  cat("n.images :",n.images,"\n")
  cat("n.dates :",n.dates,"\n")
  cat("slash :",slash,"\n")
  
  paste(slash,"documentclass{article} \n\n",
        "% Largeur papier \n",
        slash,"def",slash,"PaperWidth{",44.2,"} % 21 jours \n \n",
        "% Hauteur papier \n",
        slash,"def",slash,"PaperHeight{",18.2,"} % 5 blocs \n",
        slash,"usepackage{Packages} % Input du fichier appelant les pkgs \n",
        slash,"author{Gabriel Lemyre} \n\n",
        slash,"usepackage{hhline} \n",
        slash,"begin{document} \n",
        sep="",collapse=""
  )
}