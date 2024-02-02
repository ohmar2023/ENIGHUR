# Función para estilo de tablas -------------------------------------------

tabla_excel <- function(wb, data, hoja, titulo, tipo_letra="Century Gothic",
                        inicio_fila=3, relleno="#0072B2", 
                        letra_color="#a0a0a0",
                        letra_tamanio=10,
                        col_percent=NULL){
  
  ## Creando el archivo excel 
  addWorksheet(wb=wb, sheetName = hoja)
  writeData(wb = wb, sheet = hoja, x = data,
            startCol = 1, startRow = inicio_fila, colNames = TRUE)
  
  #Estilo para el título
  EstiloTitulo <- createStyle(halign = "center",
                              valign = "center",
                              wrapText = T,
                              fontSize = 14,
                              textDecoration = "bold", 
                              fontColour = "#002060")
  
  ## Estilo de Encabezados
  Estilocolnames <- createStyle(halign = "center",#centrar horizontal
                                valign = "center", #centrar vertical
                                wrapText = T,#ajuste del tamaño de la celda para el contenido
                                border = c("top", "bottom"), 
                                borderStyle = "thin", 
                                textDecoration = "bold", #negrita
                                fgFill = relleno, #color de relleno de la celda
                                fontSize = letra_tamanio,#tamaño de la letra
                                fontColour = letra_color) #color de la letra 
  
  #eSTILO PARA EL CONTENIDO
  EstiloContenido <- createStyle(valign = "center",
                                 halign = "center",
                                 wrapText = T,
                                 fontSize = letra_tamanio,
                                 fontColour = letra_color,
                                 # border = c("bottom", "right","top","left"), 
                                 borderStyle = "thin" #BORDE DE PUNTOS Y GUIONES
  )
  
  #eSTILO PARA LA LINEA DEL TOTAL
  EstiloTotales <- createStyle(
    valign = "center",
    wrapText = T,
    textDecoration = "bold",
    border = c("bottom","top"), 
    borderStyle = "thin", 
    fontSize = letra_tamanio,
    fontColour = letra_color
  )
  
  ## Formato - estilo de Numeros, fechas, %
  
  #Formato de número entero
  EstiloEnteros <- createStyle(halign = "right", 
                               numFmt = "#,##0",#para que se muestre el cero como 0
                               #border = c("bottom", "right","top","left"), 
                               fontColour = letra_color,
                               fontSize = letra_tamanio)
  #Formato de número decimal
  EstiloDecimal <- createStyle(halign = "right", 
                               numFmt = "#,##0.00",#para que se muestre el cero como 0
                               #border = c("bottom", "right","top","left"), 
                               fontColour = letra_color,
                               fontSize = letra_tamanio)
  #Formato de texto
  EstiloTexto <- createStyle(halign = "center", 
                             textDecoration = "bold", #negrita
                             wrapText = T,
                             #border = c("bottom", "right","top","left"), 
                             #borderStyle = "thin",
                             fontColour = letra_color,
                             fontSize = letra_tamanio)
  #Formato de porcentaje
  EstiloPercent <- createStyle(halign = "right", 
                               numFmt = "0.00%",
                               # border = c("bottom", "right","top","left"), 
                               # borderStyle = "medium",
                               fontColour = letra_color,
                               fontSize = letra_tamanio)
  #Formato de fecha
  EstiloFecha <- createStyle(halign = "center",
                             numFmt = "mm/dd/yyyy",
                             # border = c("bottom", "right","top","left"), 
                             # borderStyle = "medium",
                             fontColour = letra_color,
                             fontSize = letra_tamanio)
  
  ## Insertando estilos
  
  #aplico el estilo al encabezado de la tabla
  addStyle(wb, 
           sheet = hoja,
           style = Estilocolnames, #nombre del estilo a aplicar
           rows = inicio_fila, #pongo las filas para aplicar el estilo
           cols = 1:ncol(data), #pongo las columnas para aplicar el estilo
           gridExpand = T,# TRUE para que el estilo se aplique a todas las combinaciones de filas y columnas.
           stack = F)#T si el nuevo estilo se fusiona con cualquier estilo de celda existente, o F si cualquier estilo existente se reemplaza por el nuevo estilo.
  
  ## Estilo de números enteros
  prev <- data %>% select_if( is.integer)
  cols_aux <- which(names(data) %in% names(prev) )
  
  addStyle(wb, sheet = hoja, 
           style = EstiloEnteros, 
           rows = (inicio_fila+1):(nrow(data)+inicio_fila),
           cols = cols_aux,
           gridExpand = T, 
           stack = FALSE)
  
  ## Estilo de decimales 
  prev <- data %>% select_if( is.double)
  cols_aux <- which(names(data) %in% names(prev) )
  
  addStyle(wb, sheet = hoja, 
           style = EstiloDecimal, 
           rows = (inicio_fila+1):(nrow(data)+inicio_fila),
           cols = cols_aux,
           gridExpand = T, 
           stack = FALSE)
  
  
  #estilo de texto
  prev <- data %>% select_if( is.character)
  cols_aux <- which(names(data) %in% names(prev) )
  
  addStyle(wb, sheet = hoja, 
           style = EstiloTexto, 
           rows = (inicio_fila+1):(nrow(data)+inicio_fila), 
           cols = cols_aux, 
           gridExpand = T, stack = FALSE)
  
  #estilo de porcentaje
  addStyle(wb, sheet = hoja, 
           style = EstiloPercent, 
           rows = (inicio_fila+1):(nrow(data)+inicio_fila), 
           cols = col_percent, 
           gridExpand = T, stack = F)
  
  #Escribo sobre el excel
  
  ## titulos
  writeData(wb,sheet = hoja,startRow = 1,startCol = 1, x = titulo)
  mergeCells(wb,sheet = hoja,rows = 1,cols = 1:8)#combinar celdas
  addStyle(wb,sheet = hoja,rows = 1,cols = 1,style = EstiloTitulo)
  
  ##Total
  
  
  #Cambio el tipo de letra a ARIAL
  modifyBaseFont(wb, fontSize = letra_tamanio, fontColour = letra_color, 
                 fontName = tipo_letra)
  
  #Modifico la altura de la celda
  #setRowHeights(wb, hoja, rows=inicio_fila, heights = 30)
  #Modifico el ancho de la celda
  setColWidths(wb, hoja, cols = 1, widths = 27 )#"auto"
  setColWidths(wb, hoja, cols = 2:ncol(data), widths = 9 )#"auto"
  
  #Apago la vista de líneas de cuadrícula
  showGridLines(wb, hoja, showGridLines = FALSE)
  
}


# ---------------------------------------------------------------------------

wb1 <- createWorkbook()


tabla_excel(wb=wb1,
            data=r_f[[1]],
            hoja="CENTRO1",
            titulo = "_CENTRO")

openxlsx::saveWorkbook(wb1,
                       "ojo.xlsx",
                       overwrite = TRUE)





