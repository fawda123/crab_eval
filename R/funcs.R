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
  browser()
  if(inherits(x, 'glm')){
    
    out <- data.frame(
      `Chi-stat` = car::Anova(x, type = 2)[[1]],
      `p-mod` = p_ast(car::Anova(x, type = 2)[[3]]),
      AIC = x$aic, 
      slope = coef(summary(x))[2, 1],
      `z-stat` = coef(summary(x))[2, 3],
      `p-slope-exact` = coef(summary(x))[2, 4],
      `p-slope` = p_ast(coef(summary(x))[2, 4]),
      stringsAsFactors = F
      )
    
  } else {
    
    out <- data.frame(
    
      `F-stat` = x$fstatistic[1],
      `p-mod` = p_ast(pf(x$fstatistic[1], x$fstatistic[2], x$fstatistic[3], lower.tail = F)),
      R2 = x$r.squared,
      slope = coefficients(x)[2, 1],
      `t-stat` = coefficients(x)[2, 3],
      `p-slope-exact` = coefficients(x)[2, 4],
      `p-slope` = p_ast(coefficients(x)[2, 4]),
      stringsAsFactors = F
      
    )
    
  }
  
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

# Data to plot for effects
#
# modin lm model
# cvar chr string of variable to hold constant
# pos is where the labels are, left or right of effects line
# fct is scaling factor for labels from end of lines
get_pldat <- function(modin, cvar, pos = c('left', 'right'), fct = NULL){
  
  pos <- match.arg(pos)
  
  # crossing of model data by range
  x <- modin$model %>% 
    .[, -1] %>% 
    data.frame %>% 
    as.list %>% 
    map(range) %>%
    map(function(x) seq(x[1], x[2], length = 100))
  
  # quantiles for cvar
  x[[cvar]] <- modin$model[[cvar]]%>% quantile(., c(0, 1))
  
  # make data frame
  nms <- names(x) 
  x <- crossing(x[[1]], x[[2]])
  names(x) <- nms
  x <- x[, c(names(x)[!names(x) %in% cvar], cvar)]
  
  # get predictions, combine with exp vars
  prd_vl <- predict(modin, newdata = x, se = T) %>% 
    data.frame(., x) %>% 
    dplyr::select(-df, -residual.scale) %>% 
    mutate(
      hi = fit + se.fit, 
      lo = fit - se.fit
    )
  names(prd_vl)[1] <- all.vars(formula(modin))[1]
  
  # min x axis values for quantile labels
  yvar <- names(prd_vl)[1]
  xvar <- all.vars(formula(modin))
  xvar <- xvar[!xvar %in% c(yvar, cvar)]
  
  locs <- prd_vl %>% 
    group_by(.dots = list(cvar))   
  if(pos == 'right'){
    if(is.null(fct)) fct <- 1.05
    locs <- filter(locs, row_number() == n())
  } else {
    if(is.null(fct)) fct <- 0.95
    locs <- filter(locs, row_number() == 1)
  }
  
  yval <- locs[[yvar]]
  xval <- locs[[xvar]] %>% unique %>% `*`(fct)
  xlab <- data.frame(
    lab = c('Max', 'Min'), 
    x = xval, y = yval,
    stringsAsFactors = F)
  dr <- locs[[cvar]] %>% range %>% diff %>% sign
  if(dr == 1) xlab$lab <- rev(xlab$lab)
  
  # output
  out <- list(prd_vl = prd_vl, xlab = xlab)
  return(out)
  
}