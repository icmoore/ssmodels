quickTimePlot <- function(yt,ci,d=FALSE){
  
  yt = as.data.frame(as.matrix(yt))
  varNms <- as.matrix(colnames(yt))
  N = nrow(yt);   
  if(d[1] == FALSE) { t = 1:N } else { t = d; }
  yt = as.data.frame(cbind(t,yt));
  colnames(yt)[1] = c("t")
  
  cbgColourPalette <- c("#292f67", "#31c242",  "#56B4E9", "#ff0000","#009E73", "#000000", "#F0E442", "#0072B2", "#D55E00");
  for(k in 2:ncol(yt))
  {
    colnames(yt)[k] = paste0("y",k-1)
  }  
  
  p =  ggplot()

  if((ci) && (ncol(yt) > 3))
  {  
    
    if(ncol(yt) > 3) { line1 = geom_line(data=yt, aes(t, y1, color = varNms[1]), linetype="solid", size = 1); p = p + line1 }
    if(ncol(yt) > 4) { line2 = geom_line(data=yt, aes(t, y2, color = varNms[2]), linetype="solid", size = 1); p = p + line2 }
    if(ncol(yt) > 5) { line3 = geom_line(data=yt, aes(t, y3, color = varNms[3]), linetype="solid", size = 1); p = p + line3 }

    if(ncol(yt) > 3) { p = p + geom_ribbon(data=yt,aes(t, ymin=y2,ymax=y3),alpha=0.2) }
    if(ncol(yt) > 4) { p = p + geom_ribbon(data=yt,aes(t, ymin=y3,ymax=y4),alpha=0.2) }
    if(ncol(yt) > 5) { p = p + geom_ribbon(data=yt,aes(t, ymin=y4,ymax=y5),alpha=0.2) }
    
  } else {
    if(ncol(yt) > 1) { line1 = geom_line(data=yt, aes(t, y1, color = varNms[1]), linetype="solid", size = 1); p = p + line1 }
    if(ncol(yt) > 2) { line2 = geom_line(data=yt, aes(t, y2, color = varNms[2]), linetype="solid", size = 1); p = p + line2 }
    if(ncol(yt) > 3) { line3 = geom_line(data=yt, aes(t, y3, color = varNms[3]), linetype="solid", size = 1); p = p + line3 }
    if(ncol(yt) > 4) { line4 = geom_line(data=yt, aes(t, y4, color = varNms[4]), linetype="solid", size = 1); p = p + line4 }
    if(ncol(yt) > 5) { line5 = geom_line(data=yt, aes(t, y5, color = varNms[5]), linetype="solid", size = 1); p = p + line5 }
  }
    
  ylabel = ylab(paste0("y"))
  legend = scale_colour_manual("Legend",values=cbgColourPalette)
  p =  p + ylabel + legend
  
  
  d = ggplot(yt)  
  if(ncol(yt) > 1) { dens1 = geom_density(aes(x = y1, color=varNms[1]),  fill=cbgColourPalette[1], size=1, alpha = 0.2); d = d + dens1 }
  if(ncol(yt) > 2) { dens2 = geom_density(aes(x = y2, color=varNms[2]), size=1, fill=cbgColourPalette[2], alpha = 0.2); d = d + dens2 }
  if(ncol(yt) > 3) { dens3 = geom_density(aes(x = y3, color=varNms[3]), size=1, fill=cbgColourPalette[3], alpha = 0.2); d = d + dens3 }
  if(ncol(yt) > 4) { dens4 = geom_density(aes(x = y4, color=varNms[4]), size=1, fill=cbgColourPalette[4], alpha = 0.2); d = d + dens4 }
  if(ncol(yt) > 5) { dens5 = geom_density(aes(x = y5, color=varNms[5]), size=1, fill=cbgColourPalette[5], alpha = 0.2); d = d + dens5 }  
  ylabel = ylab(paste0("Density"))
  xlabel = xlab(paste0("Range")) 
  d = d + ylabel + xlabel + legend
  
  out = list(p=p,d=d)
  
  return(out)
}