# ------------------------------------------------------------------------------
# FUNCION 1
# CALCULOS POR ZONALES considerando los equipos de 4 personas
# Retorna dos enteros: Uno con el n final y el otro con el aumento total
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
    print(paste(n_zonal_n,enc_aumento))
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
  



