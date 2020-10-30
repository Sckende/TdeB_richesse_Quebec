# Fonction pour formatage des données et application de la fonction iNEXT
rare_shiny <- function(x){
  groups <- unique(x$species_gr)
  
  freq <- list()
  
  for(i in 1:length(groups))
  {
    
    sub <- subset(x,
                  x$species_gr == groups[i])
    
    freq[[i]] <- sort(table(sub$scientific_name),
                      decreasing = TRUE)
    
  }
  
  names(freq) <- groups
  
  rare <- iNEXT(freq, q=0,
                datatype="incidence_freq")
  
  #================================================================================
  
  # Réalisation du tableau
  
  #================================================================================
  
  return(rare$AsyEst[rare$AsyEst$Diversity=="Species richness",c(1,3,4,6,7)])
}