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
# Copie le contenu d'une variable de type 'string' au presse-papier de l'usager
# ——————————————————————————————————————————————————————————————————————————
Copie.Presse.Papier <- function(string) {
  os <- Sys.info()[['sysname']]
  if (os == "Windows") { # Si systeme dexploitation windows
    return(utils::writeClipboard(string))
  } else if (os == "Darwin") { # Si systeme dexploitation iOS
    Mac.Copie.Presse.Papier <- function(string){
      presse.papier <- pipe("pbcopy", "w")
      cat(string, file = presse.papier, sep = "\n")
      close(presse.papier)	# Fermer lobjet presse-papier
    }
    return(Mac.Copie.Presse.Papier(string))
  }
}


# ——————————————————————————————————————————————————————————————————————————
# Transforme un nombre en son caractère associé
# ——————————————————————————————————————————————————————————————————————————
chr <- function(n) { rawToChar(as.raw(n)) }
