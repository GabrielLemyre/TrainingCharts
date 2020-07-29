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
    mat.ord.liste.images[i,] <- c(as.numeric(number.split)*1,liste.images[i])
  }
  cat(hline(),"\n")
  ord.list.img.df <- as.data.frame(cbind(mat.ord.liste.images,as.numeric(mat.ord.liste.images[,1])-c(1,3,3:n)))
  names(ord.list.img.df) <- c("numero","path","id")
  print(ord.list.img.df)
  print(typeof(ord.list.img.df$id))
  
  liste.img.finale <- ord.list.img.df[order(as.numeric(ord.list.img.df$id)),]
  cat(" ",order(ord.list.img.df$id),"\n")
  print(liste.img.finale)
  
  liste.noms <- liste.noms[order(as.numeric(ord.list.img.df$id))]
  print(liste.noms)
  
  return(list(liste.noms=liste.noms,
              liste.img.finale=liste.img.finale,
              liste.nb.rows=liste.nb.rows[order(as.numeric(ord.list.img.df$id))]))
}
