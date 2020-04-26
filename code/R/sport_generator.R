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

# ——————————————————————————————————————————————————————————————————————————
# FONCTION PERMETTANT D'IMPRIMER LE RÉSULTATS EN FORMAT LATEX
# ——————————————————————————————————————————————————————————————————————————
gen.sport <- function(image,
                      titre=NULL, 
                      dates=NULL, 
                      len=NULL, cell.w.cm=1.4, nbrows=3, slash=chr(92), 
                      total.line=FALSE,
                      image.width=3){
  
  if (is.null(len)){
    n <- length(dates)
    Final.str <- ""
  } else {
    n <- len
    Final.str <- paste0("~",slash,slash," \n\n")
  }
  
  jour.sem <- c("dimanche","lundi","mardi","mercredi","jeudi","vendredi","samedi")
  mois.anne <- c("janv.", 	"févr.", 	"mars", 	"avr.", 	"mai", 	"juin", 	"juil.",  	"août", 	"sept.", 	"oct.", 	"nov.", 	"déc.")
  
  Final.str <- paste(Final.str,
                     '% ------------------------ \n',
                     slash,'noindent \n',
                     slash,"begin{minipage}{",
                     slash,"linewidth} \n",
                     slash,"mbox{}",slash,"par \n",     
                     slash,"begin{tabular}{|*{10}{c}*{",n,"}{||c}|} \n",sep="",collapse="")
  
  first.hhline <- paste("hhline{*{10}{~}*{",n,"}{-}}",sep="",collapse="")
  hhline <- paste("hhline{*{10}{~}*{",n,"}{:=:}}",sep="",collapse="")
  hhline.last <- paste("hhline{*{10}{-}*{",n,"}{|~|}}",sep="",collapse="")
  
  # FIRST LINE OF SPORT
  if (is.null(len)){
    # print("YES dates")
    dates.string <- paste(" & ",slash,"thead{",jour.sem[wday(dates)]," ",slash,slash,slash,"hline ",mday(dates)," ",mois.anne[month(dates)]," $_{",slash,"text{",year(dates),"}}$}",sep="",collapse="")
    
    Final.str <- paste(Final.str,
                       slash,first.hhline,"\n",
                       slash,"multicolumn{10}{c|}{}",dates.string,slash,slash," \n",
                       slash,hhline,"\n",sep="",collapse="")
  } else {
    # print("NO dates")
    Final.str <- paste(Final.str,     
                       slash,first.hhline,"\n",sep="",collapse="")
  }
  
  mult.10 <- "multicolumn{10}{l|"
  
  # 
  n.multirow <- switch(as.character(nbrows),"2"=6/nbrows,"1"=6/nbrows,2)
  
  # EMPTY LINE
  ligne.vide <- function(last.line=FALSE){
    res <- paste0(
      slash,mult.10,"}{}          ",
      paste0(rep("& ",n),
             collapse=""),
      slash,slash," ",if(last.line){paste0(slash,if(last.line==2){hhline.last}else{hhline},collapse="")}," \n",
      collapse="")
    return(res)
  }
  
  # CALENDAR ROW
  calendar.row <- paste0(
    slash,mult.10,"}{}                  ",
    paste0(
      rep(
        paste0(
          "& ",
          slash,"multirow{",n.multirow,"}{*}{",slash,"hspace{",cell.w.cm,"cm}}",
          collapse=""),n),
      collapse=""),slash,slash," \n",
    paste0(rep(ligne.vide(0),
               n.multirow-2),
           ligne.vide(1),
           collapse=""),
    collapse="")
  
  # LIGNE AVEC IMAGE
  premiere.ligne <- paste0(slash,mult.10,"}{",
                           slash,"multirow{",2*nbrows-1,"}{*}{",
                           slash,'includegraphics[width=',image.width,'cm]{"',image,'"}}} ',paste0(
                             rep(paste0(
                               "& ",slash,"multirow{",n.multirow,"}{*}{",slash,"hspace{",cell.w.cm,"cm}}",
                               collapse=""),n),
                             collapse=""),slash,slash," \n",
                           paste0(
                             rep(ligne.vide(0),
                                 n.multirow-if(nbrows>1){2}else{3}),if(nbrows>1){ligne.vide(1)},
                             collapse=""),
                           collapse="")
    
  # ASSEMBLAGE DU TABLEAU
  Final.str <- paste0(Final.str,
                  premiere.ligne,
                  if(nbrows>1){
                    paste0(rep(calendar.row,
                               (nbrows-2)),collapse="")
                  },
                  if(nbrows>1){paste0(rep(ligne.vide(0),
                                          n.multirow-2),
                                      collapse="")},
                  ligne.vide(2),
                  slash,substr(mult.10,1,nchar(mult.10)-2),"|l||}{",titre,"}       ",
                  paste0(rep("& ",n),
                         collapse=""),slash,slash," ",slash,"hline \n",
                  slash,"end{tabular} \n",
                  slash,"end{minipage} \n",
                  "% ----------------------------------------------------- \n",collapse="")
  
  
  if (total.line){
    # Final.str <- paste(Final.str,sep="",collapse="")
    cat("TODO : total.line=TRUE\n")
  }
  
  # Fin de lexecution et retour du resultat
  Copie.Presse.Papier(Final.str) # Copie le string concatene au presse-papier
  
  # str=cat(Final.str) # Limprime aussi dans la console R
  
  return(Final.str=Final.str)
}

