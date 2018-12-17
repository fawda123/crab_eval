######
# get legend from an existing ggplot object
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

p_ast <- function(x){
  
  sig_cats <- c('**', '*', 'ns')
  sig_vals <- c(-Inf, 0.01, 0.05, Inf)
  
  out <- cut(x, breaks = sig_vals, labels = sig_cats, right = FALSE)
  out <- as.character(out)
  
  return(out)
  
}

# summary function for regression mods
getsum <- function(x){
  
  out <- data.frame(
  
    `F-stat` = x$fstatistic[1],
    `p-mod` = p_ast(pf(x$fstatistic[1], x$fstatistic[2], x$fstatistic[3], lower.tail = F)),
    R2 = x$r.squared,
    slope = coefficients(x)[2, 1],
    `t-stat` = coefficients(x)[2, 3],
    `p-slope` = p_ast(coefficients(x)[2, 4]),
    stringsAsFactors = F
    
  )
  
  return(out)
  
}

######
# get delta and depth-integrated environmental values
# original script from raw/Dunge_crab_env.R
calc.1.V <- function( data.df, CTD.df, V.name, Z.D, Z.0, Z.range, Z.layer ) {
  n.stn <- nrow( data.df )
  
  V <- list( D.V = rep( NA, n.stn), V.mean = rep( NA, n.stn) )
  for( k.stn in 1:n.stn ) {
    indx.stn <- ( CTD.df$Sta == data.df$Stn[k.stn] )
    indx.stn[is.na(indx.stn)] <- FALSE
    CTD.1.stn <- CTD.df[ indx.stn, c("Press",V.name ) ]
    CTD.1.stn[ CTD.1.stn < -900 ] <- NA
    indx.fin <- is.finite( CTD.1.stn[,"Press"] ) & is.finite( CTD.1.stn[,V.name] )
    CTD.1.stn <- CTD.1.stn[ indx.fin, ]
    if( nrow( CTD.1.stn ) == 0 ) next
    #Calculate V in the depths of interest: Z.D, Z.range, Z.range[2]-Z.layer
    V.appr <- approx( x = CTD.1.stn[,"Press"], y = CTD.1.stn[,V.name], rule = 2,
                      xout = c( Z.D, Z.0, Z.range, Z.range[2]-Z.layer))
    V.appr <- as.data.frame( matrix(unlist(V.appr),ncol=2) )
    names(V.appr) <- c( "Press", V.name )
    #
    V$D.V[k.stn] <- V.appr[2,V.name] - V.appr[1,V.name]
    #
    CTD.1.stn <- rbind( CTD.1.stn, V.appr[3:5,] )
    indx.sort <- order( CTD.1.stn$Press )
    CTD.1.stn <- CTD.1.stn[ indx.sort, ]
    indx.trapz <- ( CTD.1.stn$Press >= (Z.range[2]-Z.layer) ) & ( CTD.1.stn$Press <= Z.range[2] )
    V$V.mean[k.stn] <- pracma::trapz( x = CTD.1.stn[indx.trapz,"Press"], 
                                      y = CTD.1.stn[indx.trapz,V.name] ) / Z.layer
  }
  return( V )
}