

make_rda <- function(hf_sites_data, clim_sites_data, data_mat){
  
  human_foot <- mapselector::human_footprint
  # climat_dat <- mapselector::
  
  # Préparation des données
  X <- cbind(hf_sites_data[,2], clim_sites_data[,3]/10)
  Y <- decostand(data_mat, method = "hellinger")
  
  # Création du modèle RDA
  Yhat <- X %*% solve((t(X) %*% X)) %*% t(X) %*% Y
  
  # Interprétation des compositions dans le nouvel espace
  Xhat <- Y %*% ginv(solve((t(X) %*% X)) %*% t(X) %*% Y)
}