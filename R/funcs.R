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