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

gen.sport <- function(image,titre=NULL, dates=NULL, len=NULL, cell.w.cm=1.4, nbrows=3, slash=chr(92), total.line=TRUE){
  
  if (is.null(len)){
    print("YES dates")
    n <- length(dates)
  } else {
    print("NO dates")
    n <- len
  }
  
  image.width <- nbrows
  jour.sem <- c("dimanche","lundi","mardi","mercredi","jeudi","vendredi","samedi")
  mois.anne <- c("janv.", 	"févr.", 	"mars", 	"avr.", 	"mai", 	"juin", 	"juil.",  	"août", 	"sept.", 	"oct.", 	"nov.", 	"déc.")
    
  Final.str <- paste('% ------------------------ \n',
                     slash,'noindent \n',
                     slash,"begin{minipage}{",
                     slash,"linewidth} \n",
                     slash,"mbox{}",slash,"par \n",     
                     slash,"begin{tabular}{|*{10}{c}*{",n,"}{||c}|} \n",sep="",collapse="")
  
  first.hhline <- paste("hhline{*{10}{~}*{",n,"}{-}}",sep="",collapse="")
  hhline <- paste("hhline{*{10}{~}*{",n,"}{:=:}}",sep="",collapse="")
  hhline.last <- paste("hhline{*{10}{-}*{",n,"}{|~|}}",sep="",collapse="")
  
  if (is.null(len)){
    print("YES dates")
    dates.string <- paste(" & ",slash,"thead{",jour.sem[wday(dates)]," ",slash,slash,slash,"hline ",mday(dates)," ",mois.anne[month(dates)]," $_{",slash,"text{",year(dates),"}}$}",sep="",collapse="")
    
    Final.str <- paste(Final.str,
                       slash,first.hhline,"\n",
                       slash,"multicolumn{10}{c|}{}",dates.string,slash,slash," \n",
                       slash,hhline,"\n",sep="",collapse="")
  } else {
    print("NO dates")
    Final.str <- paste(Final.str,     
                       slash,first.hhline,"\n",sep="",collapse="")
  }
  
  mult.10 <- "multicolumn{10}{l|"
  
  
  calendar.row <- paste(slash,mult.10,"}{}                  ",paste(rep(paste("& ",slash,"multirow{2}{*}{",slash,"hspace{",cell.w.cm,"cm}}",sep="",collapse=""),n),sep="",collapse=""),slash,slash," \n",
                        slash,mult.10,"}{}                  ",paste(rep("& ",n),sep="",collapse=""),slash,slash," ",slash,hhline, "\n",sep="",collapse="")
  
  charter <- function(){
    return(paste(Final.str,
                     slash,mult.10,"}{",slash,"multirow{",2*nbrows-1,"}{*}{",slash,'includegraphics[width=',image.width,'cm]{"',image,'"}}} ',paste(rep(paste("& ",slash,"multirow{2}{*}{",slash,"hspace{",cell.w.cm,"cm}}",sep="",collapse=""),n),sep="",collapse=""),slash,slash," \n",
                     slash,mult.10,"}{}                  ",paste(rep("& ",n),sep="",collapse=""),slash,slash," ",slash,hhline," \n",
                     paste(rep(calendar.row,(nbrows-2)),sep="",collapse=""),
                     slash,mult.10,"}{}                  ",paste(rep(paste("& ",slash,"multirow{2}{*}{",slash,"hspace{",cell.w.cm,"cm}}",sep="",collapse=""),n),sep="",collapse=""),slash,slash," ",slash,hhline.last,"\n",
                     slash,substr("multicolumn{10}{l|",1,nchar(mult.10)-2),"|l||}{",titre,"}       ",paste(rep("& ",n),sep="",collapse=""),slash,slash," ",slash,"hline \n",
                     slash,"end{tabular} \n",
                     slash,"end{minipage} \n",
                     "~",slash,slash," \n",
                     "% ----------------------------------------------------- \n \n",sep="",collapse=""))
  }
  
  Final.str <- charter()
  
  if (total.line){
    # Final.str <- paste(Final.str,sep="",collapse="")
    print("TODO : total.line=TRUE")
  }
  
  # Fin de lexecution et retour du resultat
  Copie.Presse.Papier(Final.str) # Copie le string concatene au presse-papier
  
  # str=cat(Final.str) # Limprime aussi dans la console R
  
  return(Final.str=Final.str)
}

