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
                      nbrows, slash=chr(92), 
                      total.line=FALSE,
                      image.width=3,
                      is.text=F,
                      numero.segment){
  
  print(nbrows)
  
  slash <- chr(92)
  linebreak <- paste0(slash,slash)
  
  # Nombre de cellules horizontale
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
  
  # Hauteur de chaque cellule verticale
  cat("nbrows=",as.character(nbrows),"\n")
  n.multirow <- switch(as.character(nbrows),"1"=6/nbrows,"2"=6/nbrows,2)
  
  Form.field.cell.builder <- function(width,
                                      height,
                                      activite,
                                      ligne,
                                      colonne){
    # ——————————————————————————————————————————
    paste0("& ",slash,"multirow{",height,"}{*}{",slash,"centering ",
           slash,"TextField[align=1,width=",width,"cm, name='activite.",activite,"_ligne.",ligne,"_colonne.",colonne,"',tabkey=",ligne*colonne,",bordersep=0,bordercolor=red]{}",
           "}%\n")
  }
  
  
  
  # ———————————————————————————————————
  # Construction d'une ligne de tableau
  # ———————————————————————————————————
  # USE la valeur maximale (nbrows) au lieu de id.ligne==2, comme ça je vais pouvoir utiliser le numero de ligne pour 
  construction.ligne.tableau <- function(id.ligne=0,
                                         n.horizontal,
                                         etiquette.ligne="vide", # ou "titre", "image"
                                         no.activite=1,
                                         no.ligne=1,
                                         width=1,
                                         height=2){
    # Initialisation string pour cellules
    string.item <- ""
    # Si le titre n'est pas présent déjà, on ajoute la bonne commande multicolumn 
    prefix.item.tableau <- switch(etiquette.ligne,
                                  "vide"=paste0(multicolumn.10,"}{}          "),
                                  "titre"="",
                                  "image"=paste0(multicolumn.10,"}{",
                                                 slash,"multirow{",2*max(nbrows,3)-1,"}{*}{",
                                                 slash,"parbox{",image.width,"cm}{",
                                                 if (is.text){
                                                   paste0(slash,"centering ",slash,"LARGE ",image)
                                                 } else {
                                                   paste0(slash,'includegraphics[width=',image.width,'cm]{"',image,'"} ')
                                                 },"}}}"))
    
    
    
    # Construction de la string pour la ligne.
    #   Ajout de "n.horizontal" cellules avec boite de formulaire ou vide si pas de formulaire demandé
    if (id.ligne==3){
      for (no.colonne in 1:n.horizontal){
        print(no.colonne)
        string.item <- paste0(string.item,
               Form.field.cell.builder(width=width,height=height,
                                       activite=no.activite,
                                       ligne=no.ligne,colonne=no.colonne),
               collapse="")
      }
    } else {
      string.item <- paste0(rep("& ",n.horizontal),
                            collapse="")
    }
  
    item.tableau <- paste0(prefix.item.tableau,string.item,collapse="")
    
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
  } # fin fonction construction.ligne.tableau
  
  
  # ———————————————————————————————————
  # CALENDAR ROW
  # ———————————————————————————————————
  calendar.row <- function(no.ligne,
                           no.activite,
                           width,
                           n.vide,
                           last.row){
    
    last.row.string <- ""
    if (last.row){
      print("GOGO")
      last.row.string <- construction.ligne.tableau(id.ligne=0,
                                                    n.horizontal=n.horizontal,
                                                    height=n.multirow-1)
    }
    
    paste0(
      paste0(if (n.vide>2){
               rep(construction.ligne.tableau(id.ligne=0,
                                              n.horizontal=n.horizontal,
                                              height=n.multirow-1),
                   n.vide-2)
               },
        collapse=""),
      # last.row.string,
      collapse="")
  }
  
  calendar.bloc <- paste0(construction.ligne.tableau(id.ligne=3,
                                                     etiquette.ligne="image",
                                                     n.horizontal=n.horizontal,
                                                     no.activite=numero.segment,
                                                     no.ligne=1,
                                                     width=cell.w.cm,
                                                     height=if(nbrows==1){n.multirow-1}else{1}))
  if (nbrows>1){
    for (a in 2:nbrows){
      if (a==nbrows){
        last.row <- T
        calendar.bloc <- calendar.bloc %>% 
          paste0(calendar.row(no.ligne=a,
                              no.activite=numero.segment,
                              width=cell.w.cm,
                              n.vide=n.multirow-1,
                              last.row=last.row))
      } else {
        last.row <- F
        calendar.bloc <- calendar.bloc %>% 
          paste0(calendar.row(no.ligne=a,
                              no.activite=numero.segment,
                              width=cell.w.cm,
                              n.vide=n.multirow,
                              last.row=last.row))
      }
    }
  } else {
    calendar.bloc <- calendar.bloc %>% 
      paste0(calendar.row(no.ligne=1,
                          no.activite=numero.segment,
                          width=cell.w.cm,
                          n.vide=n.multirow-1,
                          last.row=T))
  }
  
  
  cat(calendar.bloc)
  
  # ———————————————————————————————————
  # DATES ROW
  # ———————————————————————————————————
  # Construction de la chaine de caractères pour les dates
  dates.row <- "" # Initialisation string pour les dates, reste vide si pas de dates à imprimer
  if (is.null(longueur.date)){
    # Construction string de dates
    dates.string <- paste(" & ",slash,"thead{",jour.sem[wday(dates)]," ",linebreak,slash,"hline ",mday(dates)," ",mois.anne[month(dates)]," $_{",slash,"text{",year(dates),"}}$}",sep="",collapse="")
    # Ajout des dates, et de la ligne separatrice à la string finale
    dates.row <- paste(slash,"multicolumn{10}{p{",image.width,"cm}|}{}",dates.string,linebreak," \n",
                       hhline,"\n",sep="",collapse="")
  }
  
  
  # ———————————————————————————————————
  # CONSTRUCTION DU SEGMENT
  # ———————————————————————————————————
  Final.str <-  Final.str %>% 
    # Ajout du tag DEBUT pour l'environnement TABULAR
    paste0('% ------------------------ \n',
           slash,'noindent \n',
           slash,"begin{minipage}{",
           slash,"linewidth} \n",
           slash,"mbox{}",slash,"par \n",  
           slash,"begin{Form} \n",   
           slash,"begin{tabular}{|*{10}{p{",image.width,"cm}}*{",n.horizontal,"}{||p{",cell.w.cm,"cm}}|} \n",collapse="") %>% 
    # 
    # Ajout de la première hhline du segment
    paste0(first.hhline,"\n",collapse="") %>% 
    # 
    # Ajout des dates
    paste0(dates.row) %>%
    # 
    # 
    # paste0(construction.ligne.tableau(id.ligne=1,
    #                                   etiquette.ligne="image",
    #                                   n.horizontal=n.horizontal,
    #                                   FormField=T,
    #                                   no.activite=numero.segment,
    #                                   no.ligne=1,
    #                                   width=cell.w.cm,
    #                                   height=n.multirow)) %>%
    # 
    # 
    paste0(calendar.bloc) %>%
    # 
    # Ajout du tag FIN pour l'environnement TABULAR
    paste0(strsplit(multicolumn.10,"p")[[1]][1],"|l||}{",titre,"}       ",
           construction.ligne.tableau(id.ligne=2,
                                      etiquette.ligne="titre",
                                      n.horizontal=n.horizontal,
                                      height=1),
           slash,"hline \n",
           slash,"end{tabular} \n",  
           slash,"end{Form} \n",   
           slash,"end{minipage} \n",
           "% ----------------------------------------------------- \n",collapse="")
  
  
  if (total.line){
    # Final.str <- paste(Final.str,sep="",collapse="")
    cat("TODO : total.line=TRU\n")
  }
  
  
  
  # Fin de lexecution et retour du resultat
  Copie.Presse.Papier(Final.str) # Copie le string concatene au presse-papier
  
  # str=cat(Final.str) # Limprime aussi dans la console R
  
  return(Final.str)
}

