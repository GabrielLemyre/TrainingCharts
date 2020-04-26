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

preambule <- function(n.images,
                      n.dates,
                      n.rows,
                      slash,
                      image.height=3,
                      image.width=3,
                      arraystretch=1){
  
  # cat("n.images :",n.images,"\n")
  # cat("n.dates :",n.dates,"\n")
  # cat("slash :",slash,"\n")
  
  padding.horizontal <- image.width+0.5
  page.width <- padding.horizontal + n.dates*1.920455
  page.height <- (n.images-1)*0.5+(n.rows+1)*0.9368182
  
  paste(slash,"documentclass{article} \n\n",
        "% Largeur papier en cm\n",
        slash,"def",slash,"PaperWidth{",page.width,"} % 21 jours \n \n",
        "% Hauteur papier en cm\n",
        slash,"def",slash,"PaperHeight{",page.height,"} % 5 blocs \n\n",
        slash,"usepackage{Packages} % Input du fichier appelant les pkgs \n",
        slash,"renewcommand{",slash,"arraystretch}{",arraystretch,"} \n",
        slash,"author{Gabriel Lemyre} \n\n",
        slash,"begin{document} \n",
        sep="",collapse=""
  )
}