# ----------------------------------------------------------------------------------------------------
# Making LaTeX Table envir. compatible code from R dataframes
# ----------------------------------------------------------------------------------------------------
# written
# Gabriel LEMYRE
# ----------------------------------------------------------------------------------------------------
# First version : lundi, 20 avril 2020
# ----------------------------------------------------------------------------------------------------
# Last version : lundi, 20 avril 2020
# ----------------------------------------------------------------------------------------------------

# ——————————————————————————————————————————————————————————————————————————
# FONCTION PERMETTANT D'IMPRIMER LE RÉSULTATS EN FORMAT LATEX
# ——————————————————————————————————————————————————————————————————————————

gen.sport <- function(image,titre=NULL, dates, len=NULL){
  
  jour.sem <- c("dimanche","lundi","mardi","mercredi","jeudi","vendredi","samedi")
  mois.anne <- c("janv.", 	"févr.", 	"mars", 	"avr.", 	"mai", 	"juin", 	"juil.",  	"août", 	"sept.", 	"oct.", 	"nov.", 	"déc.")
  cell.w.cm <- 1.4
  
  Final.str <- paste("% ------------------------ \n",
                     chr(92),"noindent \n",
                     chr(92),"begin{minipage}{",
                     chr(92),"linewidth} \n",
                     chr(92),"mbox{}",chr(92),"par \n",sep="",collapse="")
  str.for.width <- paste(chr(92),"thead{",jour.sem[wday(dates[1])]," ",chr(92),chr(92),chr(92),"hline ",mday(dates[1])," ",mois.anne[month(dates[1])]," $_{",chr(92),"text{",year(dates[1]),"}}$}",sep="",collapse="")
  
  if (is.null(len)){
    print("YES dates")
    n <- length(dates)
    dates.string <- paste(" & ",chr(92),"thead{",jour.sem[wday(dates)]," ",chr(92),chr(92),chr(92),"hline ",mday(dates)," ",mois.anne[month(dates)]," $_{",chr(92),"text{",year(dates),"}}$}",sep="",collapse="")
    
    Final.str <- paste(Final.str,     
                       chr(92),"begin{tabular}{|c|c|c|c|c|c|c|c|c|c*{",n,"}{||c}||} \n",
                       chr(92),"hhline{*{10}{~}*{",n,"}{-}} \n",
                       chr(92),"multicolumn{10}{|c|}{}",dates.string,chr(92),chr(92)," \n",
                       chr(92),"hhline{*{10}{-}*{",n,"}{=}} \n",sep="",collapse="")
  } else {
    print("NO dates")
    n <- len
    Final.str <- paste(Final.str,     
                       chr(92),"begin{tabular}{|c|c|c|c|c|c|c|c|c|c*{",n,"}{||c}||} \n",
                       chr(92),"hhline{*{10}{-}*{",n,"}{-}} \n",sep="",collapse="")
  }
  
  Final.str <- paste(Final.str,
                     chr(92),"multicolumn{10}{|l|}{",chr(92),"multirow{5}{*}{",chr(92),'includegraphics[width=3cm]{"',image,'"}}} ',paste(rep(paste("& ",chr(92),"multirow{2}{*}{",chr(92),"hspace{",cell.w.cm,"cm}}",sep="",collapse=""),n),sep="",collapse=""),chr(92),chr(92)," \n",
                     chr(92),"multicolumn{10}{|l|}{}                  ",paste(rep("& ",n),sep="",collapse=""),chr(92),chr(92)," ",chr(92),"hhline{*{10}{~}*{",n,"}{=}} \n",
                     chr(92),"multicolumn{10}{|l|}{}                  ",paste(rep(paste("& ",chr(92),"multirow{2}{*}{",chr(92),"hspace{",cell.w.cm,"cm}}",sep="",collapse=""),n),sep="",collapse=""),chr(92),chr(92)," \n",
                     chr(92),"multicolumn{10}{|l|}{}                  ",paste(rep("& ",n),sep="",collapse=""),chr(92),chr(92)," ",chr(92),"hhline{*{10}{~}*{",n,"}{=}} \n",
                     chr(92),"multicolumn{10}{|l|}{}                  ",paste(rep(paste("& ",chr(92),"multirow{2}{*}{",chr(92),"hspace{",cell.w.cm,"cm}}",sep="",collapse=""),n),sep="",collapse=""),chr(92),chr(92)," ",chr(92),"cline{1-10}  \n",
                     chr(92),"multicolumn{10}{|l|}{",titre,"}         ",paste(rep("& ",n),sep="",collapse=""),chr(92),chr(92)," ",chr(92),"hline \n",
                     chr(92),"end{tabular} \n",
                     chr(92),"end{minipage} \n",
                     "~",chr(92),chr(92)," \n",
                     "% ----------------------------------------------------- \n \n",sep="",collapse="")
  
  # Fin de lexecution et retour du resultat
  Copie.Presse.Papier(Final.str) # Copie le string concatene au presse-papier
  
  # str=cat(Final.str) # Limprime aussi dans la console R
  
  Final.str
}

