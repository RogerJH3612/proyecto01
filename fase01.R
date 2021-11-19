propagacion <- function(x,y){
  z <- ceiling(length(y)/2)
  tabla <- data.frame(x,y)
  tabla[ ,"valores_de_g"] <- 4*pi^2*x/y^2
  tabla[z,"promedio_de_g"] <- mean(tabla$valores_de_g)
  tabla[z,"sigma_g"] <- sd(tabla$valores_de_g)/sqrt(length(x))
  #print(tabla) 
  # poner print para ver la tabla
  
  colnames(tabla) <- c("$L$(m)","$T$(s)","$g$(m/s$^2$)","$\\bar{g}$(m/s$^2$)","$\\sigma_{\\bar{g}}$(m/s$^2$)")
  
  library(xtable)   # en la consola
  A <- xtable(tabla, digits = c(0,3,3,5,5,4),caption = "Analisis $g$" )    
  #  xtable mostrara el codigo
  # cuando estemos en .rnw no poner 
  # print(tabla)
  print(A, sanitize.text.function=function(x){x})
}
longitudes <- c(0.512,0.597,0.682,0.797,0.883)
periodos <- c(1.448,1.566,1.669,1.804,1.896)
propagacion(longitudes,periodos)