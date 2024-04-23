v2 <- rep("na",255)
 for ( i in c(1:255) ) {
  nombre <- names(base)[i]
  v2[i] <- (attr(base[[nombre]],"label"))
}

v2 <- tolower(v2)
  
v2[grepl(v2,pattern = "arrien|alqui|renta")]
attr(base$identif_2010,"label")
base$nombre
