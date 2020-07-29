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
                      longueur.date=NULL, 
                      cell.w.cm=1.4, 
                      nbrows=3, slash=chr(92), 
                      total.line=FALSE,
                      image.width=3,
                      is.text=F,
                      numero.segment){
  
  slash <- chr(92)
  linebreak <- paste0(slash,slash)
  
  if (is.null(longueur.date)){
    n.horizontal <- length(dates)
    Final.str <- ""
  } else {
    n.horizontal <- longueur.date
    Final.str <- paste0("~",linebreak," \n\n")
  }
  
  jour.sem <- c("dimanche","lundi","mardi","mercredi","jeudi","vendredi","samedi")
  mois.anne <- c("janv.", 	"févr.", 	"mars", 	"avr.", 	"mai", 	"juin", 	"juil.",  	"août", 	"sept.", 	"oct.", 	"nov.", 	"déc.")
  
  first.hhline <- paste(slash,"hhline{*{10}{~}*{",n.horizontal,"}{-}}",sep="",collapse="")
  hhline <- paste(slash,"hhline{*{10}{~}*{",n.horizontal,"}{:=:}}",sep="",collapse="")
  hhline.last <- paste(slash,"hhline{*{10}{-}*{",n.horizontal,"}{|~|}}",sep="",collapse="")
  
  multicolumn.10 <- paste0(slash,"multicolumn{10}{p{",image.width,"cm}|")
  
  n.multirow <- switch(as.character(nbrows),"2"=6/nbrows,"1"=6/nbrows,2)
  
  Form.field.cell.builder <- function(width=1,
                                      activite=1,ligne=1,colonne=1){
    paste0("& ",slash,"multirow{",n.multirow,"}{*}{",slash,"centering ",
           slash,"TextField[align=1,width=",width,"cm, name='activite.",activite,"_ligne.",ligne,"_colonne.",colonne,"',tabkey=",ligne*colonne,",bordersep=0,bordercolor=red]{}",
           "}%\n")
  }
  
  # EMPTY LINE
  # USE la valeur maximale (nbrows) au lieu de id.ligne==2, comme ça je vais pouvoir utiliser le numero de ligne pour 
  construction.ligne.tableau <- function(id.ligne=0,
                                         n.horizontal,
                                         FormField=F,
                                         ligne.titre=F,
                                         no.activite=1,
                                         no.ligne=1,
                                         width=1){
    # Initialisation string pour cellules
    string.item <- ""
    # Si le titre n'est pas présent déjà, on ajoute la bonne commande multicolumn 
    prefix.item.tableau <- if (!ligne.titre){paste0(multicolumn.10,"}{}          ")} else {""}
    
    # Construction de la string pour la ligne.
    #   Ajout de "n.horizontal" cellules avec boite de formulaire ou vide si pas de formulaire demandé
    item.tableau <- paste0(prefix.item.tableau,
                           if (id.ligne==0){
                             paste0(rep("& ",n.horizontal),
                                    collapse="")
                           } else {
                             for (index.colonne in 1:n.horizontal){
                               paste0(string.item,Form.field.cell.builder(width=width,activite=no.activite,ligne=no.ligne,colonne=index.colonne),
                                      collapse="")
                             }
                           }
    )
      
    # Ajout aucune ligne si cellule intermédiaire  (id.ligne = 0),
    #     double ligne en dessous si dans le corps (id.ligne = 1),
    #         ligne simple si dernière ligne       (id.ligne = 2).
    res <- paste0(if(id.ligne!=0){
      paste0(item.tableau,linebreak," ",
             if(id.ligne==1){
               hhline
             }else if(id.ligne==2){
               hhline.last
             },"\n",
             collapse="")
    }else{
      paste0(item.tableau,linebreak,"\n ")
    },
    collapse="")
    
    return(res)
  }
  
  
  # DÉBUT CONSTRUCTION DU SEGMENT
  Final.str <- paste(Final.str,
                     '% ------------------------ \n',
                     slash,'noindent \n',
                     slash,"begin{minipage}{",
                     slash,"linewidth} \n",
                     slash,"mbox{}",slash,"par \n",  
                     slash,"begin{Form} \n",   
                     slash,"begin{tabular}{|*{10}{p{",image.width,"cm}}*{",n.horizontal,"}{||p{",cell.w.cm,"cm}}|} \n",sep="",collapse="")
  
  # FIRST LINE OF SPORT
  if (is.null(longueur.date)){
    # print("YES dates")
    dates.string <- paste(" & ",slash,"thead{",jour.sem[wday(dates)]," ",linebreak,slash,"hline ",mday(dates)," ",mois.anne[month(dates)]," $_{",slash,"text{",year(dates),"}}$}",sep="",collapse="")
    
    Final.str <- paste(Final.str,
                       first.hhline,"\n",
                       slash,"multicolumn{10}{p{",image.width,"cm}|}{}",dates.string,linebreak," \n",
                       hhline,"\n",sep="",collapse="")
  } else {
    # print("NO dates")
    Final.str <- paste(Final.str,     
                       first.hhline,"\n",sep="",collapse="")
  }
  
  
  # CALENDAR ROW
  calendar.row <- function(no.ligne,
                           no.activite,
                           width,
                           n.vide){
    paste0(multicolumn.10,"}{}                  ",
    paste0(
      rep(
        paste0(
          "& ",
          slash,"multirow{",n.multirow,"}{*}{",slash,"hspace{",cell.w.cm,"cm}}",
          collapse=""),n.horizontal),
      collapse=""),
    linebreak," \n",
    paste0(rep(construction.ligne.tableau(0,n.horizontal=n.horizontal),
               n.vide),
           construction.ligne.tableau(id.ligne=1,
                                      n.horizontal=n.horizontal,
                                      FormField=T,
                                      no.activite=no.activite,
                                      no.ligne=no.ligne,
                                      width=width),
           collapse=""),
    collapse="")
  }
  # construction.ligne.tableau(id.ligne=0,
  #                            FormField=F,
  #                            ligne.titre=F,
  #                            no.activite=numero.segment,
  #                            no.ligne=1,
  #                            width=cell.w.cm)
  
  # LIGNE AVEC IMAGE
  premiere.ligne <- paste0(multicolumn.10,"}{",
                           slash,"multirow{",2*max(nbrows,3)-1,"}{*}{",
                           slash,"parbox{",image.width,"cm}{",
                           if (is.text){paste0(slash,"centering ",slash,"LARGE ",image)}else{paste0(slash,'includegraphics[width=',image.width,'cm]{"',image,'"} ')},"}}}",
                             construction.ligne.tableau(id.ligne=0,
                                                        n.horizontal=n.horizontal,
                                                        FormField=T,
                                                        no.activite=numero.segment,
                                                        no.ligne=1,
                                                        width=cell.w.cm),
                           paste0(
                             rep(construction.ligne.tableau(id.ligne=0,n.horizontal=n.horizontal),
                                 n.multirow-if(nbrows>1){2}else{3}),
                             if(nbrows>1){
                               construction.ligne.tableau(id.ligne=1,
                                                          n.horizontal=n.horizontal,
                                                          FormField=T,
                                                          no.activite=numero.segment,
                                                          no.ligne=1,
                                                          width=cell.w.cm)
                             },
                             collapse=""),
                           collapse="")
  
  # ASSEMBLAGE DU TABLEAU
  string.calendar.block <- ""
  Final.str <- paste0(Final.str,
                      premiere.ligne,
                      if(nbrows>1){
                        paste0(
                          for (h in 1:nbrows-2){
                            paste(string.calendar.block,calendar.row(no.ligne=h,
                                                                     no.activite=numero.segment,
                                                                     width=cell.w.cm,
                                                                     n.vide=n.multirow-2))
                          },
                          construction.ligne.tableau(id.ligne=2,n.horizontal=n.horizontal),collapse="")
                      } else {
                        construction.ligne.tableau(id.ligne=2,n.horizontal=n.horizontal,FormField=F)
                      },
                      strsplit(multicolumn.10,"p")[[1]][1],"|l||}{",titre,"}       ",
                      construction.ligne.tableau(ligne.titre=T,n.horizontal=n.horizontal),
                      slash,"hline \n",
                      slash,"end{tabular} \n",  
                      slash,"end{Form} \n",   
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

