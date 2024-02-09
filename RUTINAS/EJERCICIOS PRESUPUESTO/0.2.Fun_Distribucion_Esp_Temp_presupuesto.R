# ------------------------------------------------------------------------------
# Este SCRIPT solo contiene las funciones que se utilizan en el script 0.6. 
# Estas funciones nos permiten realizar la distribución espacial y temporal
# en cada dominio durante los 13 periodos en los que se llevará a cabo la
# encuesta. La distribución considera cuidar el numero de encuestadores y 
# que la muestra sea similar en cada periodo (esto en cuanto a la cantidad de 
# UPMs) por tal motivo, el tamaño de muestra aumenta en algunos dominios. Estos
# aumentos han sido aceptados por la productora. 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# FUNCIÓN 1
# CÁLCULOS POR ZONALES considerando los equipos de 4 personas.
# Retorna dos enteros: Uno con el n final y el otro con el aumento total.
# ------------------------------------------------------------------------------

fun_1 <- function(n_zonal)
{
  d_zonal <- if_else(n_zonal%%13==0,0,13-n_zonal%%13)
  n_zonal_n <- n_zonal+d_zonal
  enc_aumento <- 0
  k = (12)/(52*4)
  while (ceiling(n_zonal_n*k) - (n_zonal_n*k) != 0 ){
    enc_aumento <- enc_aumento + 13
    n_zonal_n <-  n_zonal_n + 13
    #print(paste(n_zonal_n,enc_aumento))
  }
  enc_1 <- (n_zonal_n*12)/(52*4)
  
  #Tamaño final por zonal
  aumento_total <- d_zonal + enc_aumento
  n_zonal_f <- n_zonal+aumento_total
  
  #retonro
  r <- c(n_nuevo=n_zonal_f,aumneto_total=aumento_total)
  return(r)
}

# ------------------------------------------------------------------------------
# FUNCION 2
# ASIGNAR UPMS EXTRAS
# Argumentos: v_1 -> vector con el tamaño original para cada dominio
#             aumetototal -> el numero de upms a aumentar
# Retorno: Retorna un nuevo tamaño para cada dominio
# ------------------------------------------------------------------------------

fun_2 <- function(v_1,aumento_total)
{
  v_2 <- v_1 / 13
  v_3 <- order(v_2)
  v_4 <- rep(0,length(v_1))
  i=1
  aux <- aumento_total
  while (aux != 0)
  {
    v_4[v_3[i]] <- v_4[v_3[i]]+1
    aux <- aux - 1
    i <- if_else(i==length(v_1),1,i+1)
  }
  v_5 <- v_4 + v_1 # NUEVO TAMAÑO PARA CADA PROVINCIA
  return(v_5)
}

# ------------------------------------------------------------------------------
# FUNCION 3
# n -> tamaño a distribuir 
# k -> entrada en la que debe iniciar la cinta
# Retorna una lista: un vector de la distribucion y un entero de la entrada 
# en la que se quedó la cinta
# ------------------------------------------------------------------------------

fun_3 <- function(n,k=1)
{
  periodo = 13
  n_0 <- trunc(n/periodo)
  n_f <- n - (n_0*13) #entrada en la que termina la cinta
  cinta  <- c(1,9,4,12,7,2,10,5,13,8,3,11,6)
  v_1 <- rep(n_0,13)
  j=1
  i=k #va a tener que ser un parametro
  while (j<=n_f) {
    v_1[cinta[i]] <- v_1[cinta[i]]+1
    i <- if_else(i>=length(cinta),1,i+1)
    j=j+1
  } 
  if (sum(v_1)!=n)
  {
    stop("LA DISTRIBUCION NO COINCIDE CON EL TAMAÑO")
  }
 return(list(v_1,i))
}

# ------------------------------------------------------------------------------
# FUNCION 4
# n -> original del dominio 
# v -> vector con los tamanios originales para cada provincia 
# Esta funcion abarca todas las demas funciones. Solo se deberia correr esta funcion
# ------------------------------------------------------------------------------

fun_4 <- function(n,v)
{
  v_1 <- fun_1(n)
  v_2 <- fun_2(v,v_1[2])
  
  r_4 <- NULL
  k=0
  for (j in c(1:length(v_2)))
  {
    if(j==1)
    {
      k = 1
      r_3 <- fun_3(v_2[j],k)
      r_4 <- rbind(r_4, r_3[[1]])
      #print(r_3[[1]])
    }else{
      k=r_3[[2]]  
      r_3 <- fun_3(v_2[j],k)
      r_4 <- rbind(r_4, r_3[[1]])
      #r_4[j,] <- r_3[[1]]
      #print(r_3[[1]])
    }
    colnames(r_4) = paste0("PERIODO ",c(1:13))
    #rownames(r_4) = 
  }
  return(r_4)
}
