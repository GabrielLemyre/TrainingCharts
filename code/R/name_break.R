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
name_break <- function(liste.images,n){
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
  
  return(list(liste.noms=liste.noms,
         liste.img.finale=liste.img.finale))
}
