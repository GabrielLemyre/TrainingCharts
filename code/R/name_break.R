# ----------------------------------------------------------------------------------------------------
# Making LaTeX Table envir. compatible code from R dataframes
# ----------------------------------------------------------------------------------------------------
# written
# Gabriel LEMYRE
# ----------------------------------------------------------------------------------------------------
# First version : 24 avril 2020
# ----------------------------------------------------------------------------------------------------
# Last version : 24 avril 2020
# ----------------------------------------------------------------------------------------------------

# ——————————————————————————————————————————————————————————————————————————
# FONCTION PERMETTANT D'IMPRIMER LE RÉSULTATS EN FORMAT LATEX
# ——————————————————————————————————————————————————————————————————————————
name.break <- function(liste.images){
  n <- length(liste.images)
  
  liste.noms <- matrix(NA,ncol=n)
  mat.ord.liste.images <- matrix(NA,nrow=n,ncol=2)
  liste.nb.rows <- matrix(NA,ncol=n)
  
  hline()
  cat("IMAGES RETENUES\n")
  hline()
  for (i in 1:n){
    # Obtention du nom du fichier
    file.name <- basename(liste.images[i])
    cat(file.name,"\n")
    
    split <- strsplit(strsplit(file.name,".",fixed=TRUE)[[1]][1], "_", fixed = TRUE)[[1]]
    # Obtention de son numero dans l'ordre
    number.split <- split[1]
    
    liste.noms[i] <- paste(split[1],paste(split[-c(1,length(split))],sep=" ",collapse=" "),sep=". ",collapse=" ")
    liste.nb.rows[i] <- as.numeric(split[length(split)])
    mat.ord.liste.images[i,] <- c(i,liste.images[i])
  }
  cat(hline(),"\n")

  liste.img.finale <- data.frame(mat.ord.liste.images)
  names(liste.img.finale) <- c("numero","path")
  print(liste.img.finale)
  
  liste.noms <- liste.noms
  print(liste.noms)
  
  return(list(liste.noms=liste.noms,
              liste.img.finale=liste.img.finale,
              liste.nb.rows=liste.nb.rows))
}
