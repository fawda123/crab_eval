# Dunge_crab_env.R
#   Read CTD data and calculate two environmental characteristics:
#     - The difference (delta) between two depths
#     - The parameter averaged within a layer
#
proj.dir <- "C:/Users/NikolayN/Documents/Projects/2018/Nina_regression_Dunge_crab"
data.dir <- "Data"
#
CTD.fn <- "CTD_data.csv"
CTD.df <- read.csv( file.path( proj.dir, data.dir, CTD.fn), stringsAsFactors = FALSE )
# 
# Local function returning a list with two vectors: D.V (delta) and V.mean (average)
# Vectors are calculated for 
# 
calc.1.V <- function( data.df, CTD.df, V.name, Z.D, Z.0, Z.range, Z.layer ) {
  n.stn <- nrow( data.df )
  # browser()
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

# Check the D.pH and D.T
Z.D <- 50  # Two depths for which I calculate the difference
Z.0 <- 0
# Z.range <- c(0,95)
# Z.layer <- 49
Z.range <- c(0,60)  # We average the layer of Z.layer depth at the "bottom" of the depth range Z.range
Z.layer <- 20 #  In this case, it is 40-60m (20m layer at the bottom of 0-60m depth range)
#
V.name <- "pH.TOT" # You can use other parameter from the: names(CTD.df)
# In your code, data.df should be a data frame with dissolution data, "Stn" is stations ID
#   In CTD.df data, station ID name is "Sta"
#   In the example below, I calculate D.V and V.mean for all stations, which ID is not NA
data.df <- data.frame( Stn = unique( CTD.df$Sta[!is.na(CTD.df$Sta)] ) )
#
VV <- calc.1.V( data.df, CTD.df, V.name, Z.D, Z.0, Z.range, Z.layer )
data.df$D.pH.50 <- VV$D.V
data.df$pH.mean <- VV$V.mean
#
