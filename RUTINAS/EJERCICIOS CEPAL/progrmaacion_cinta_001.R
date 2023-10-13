
# ------------------------------------------------------------------------------
# CALCULOS POR ZONALES considerando los equipos de 4 personas
# ------------------------------------------------------------------------------

#n_zonal inicial calculado
n_zonal <- 710
#Ajustamos para los 13 periodos

d_zonal <- if_else(n_zonal%%13==0,0,13-n_zonal%%13)
n_zonal_n <- n_zonal+d_zonal
#Ajustamos para los encuestadores

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

# ------------------------------------------------------------------------------
# ASIGNAR UPMS EXTRAS
# ------------------------------------------------------------------------------

#ejemplo caso ZONAL NORTE
v_1 <- c(79,215,91,253,72)

#ejemplo caso ZONAL CENTRO: Aqui hay que revisar
v_1 <- c(96,96,105,66,84,216,66)

#ejemplo caso ZONAL LITORAL
v_1 <- c(336,111,282,147,60)


#ejemplo caso ZONAL SUR
v_1 <- c(195,86,206,184,92,88)

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
  print(aux)
}
v_5 <- v_4 + v_1 # NUEVO TAMAÑO PARA CADA PROVINCIA

# ------------------------------------------------------------------------------
# FUNCION QUE DISTRIBUYE PARA CADA DOMINIO-GENERAR LA TABLA POR ZONAL EN 13 PERIODOS
# ------------------------------------------------------------------------------
periodo = 13
k = 1 #para arrancar a correr sobre la cinta
n = V_5[1]#83 #aqui va el n de cada provincia para cada zonal
n_ciu_zonal <- 5
n_0 <- trunc(n/periodo)
n_f <- n - (n_0*13) #entrada en la que termina la cinta
cinta  <- c(1,9,4,12,7,2,10,5,13,8,3,11,6)

v_1 <- rep(n_0,13)
j=1

i=k #va a tener que se run parametro
while (j<=n_f) {
  v_1[cinta[i]] <- v_1[cinta[i]]+1
  i <- if_else(i>length(cinta),1,i+1)
  j=j+1
  } 
k <- n_f
v_1













