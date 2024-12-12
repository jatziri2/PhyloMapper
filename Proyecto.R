install.packages("ape")
library(ape)

# Función principal
default_palette <- c("lightblue", "lightgreen", "orange", "pink", "purple", "gray")

mapear <- function(filogenia, datos, colores_terminales = NULL) {
  # Verificar si el archivo es tipo multiphylo
  if (inherits(filogenia, "multiPhylo")) {
    warning("El archivo de árboles es tipo multiphylo. Solamente se tomará la primera filogenia.")
    filogenia <- filogenia[[1]]
  }
  
  # Verificar si el árbol tiene longitudes de rama
  if (is.null(filogenia$edge.length)) {
    message("La filogenia no contiene longitudes de rama.")
  } else {
    message("La filogenia contiene longitudes de rama.")
  }
  
  # Transformar estados de carácter a factores
  datos[, -1] <- lapply(datos[, -1], as.factor)
  
  # Asignar paleta de colores
  if (is.null(colores_terminales)) {
    unique_states <- unique(unlist(datos[, -1]))
    colores_terminales <- setNames(default_palette[seq_along(unique_states)], unique_states)
    colores_terminales["?"] <- "darkgray"
    colores_terminales["-"] <- "red"
  }
  
  # Combinar los datos con el árbol
  datos <- datos[match(filogenia$tip.label, datos$species), ]
  
  # Verificar si hay terminales sin datos
  if (any(is.na(datos))) {
    message("Algunas terminales no tienen datos asociados. Se marcarán como datos faltantes.")
  }
  
  # Graficar el árbol
  plot(filogenia, main = "Árbol Filogenético")
  
  # Anotar las terminales con colores personalizados
  tip_colors <- colores_terminales[as.character(datos[, 2])]
  tip_colors[is.na(tip_colors)] <- "lightgray" # Color para datos faltantes
  
  tiplabels(pch = 19, col = tip_colors, cex = 1.2)
  
  # Agregar leyenda
  legend("topright", legend = names(colores_terminales), fill = colores_terminales, cex = 0.8, bty = "n")
}

mapear_multicaracter_unicos <- function(filogenia, matriz_datos, paleta_base = c("blue", "green", "yellow", "purple", "orange", "pink", "lightblue", "brown", "black")) {
  # Verificar si el archivo es tipo multiphylo
  if (inherits(filogenia, "multiPhylo")) {
    warning("El archivo de árboles es tipo multiphylo. Solamente se tomará la primera filogenia.")
    filogenia <- filogenia[[1]]
  }
  
  # Verificar si el árbol tiene longitudes de rama
  if (is.null(filogenia$edge.length)) {
    message("La filogenia no contiene longitudes de rama.")
  } else {
    message("La filogenia contiene longitudes de rama.")
  }
  
  # Verificar que el número de filas coincide con el número de terminales
  if (nrow(matriz_datos) != length(filogenia$tip.label)) {
    stop("El número de filas en la matriz de datos no coincide con el número de terminales en el árbol.")
  }
  
  # Obtener estados únicos de todos los caracteres
  estados_unicos <- unique(as.vector(matriz_datos))
  
  # Asignar colores únicos a los estados
  colores_caracter <- setNames(rep(paleta_base, length.out = length(estados_unicos)), estados_unicos)
  colores_caracter["?"] <- "darkgray" # Gris oscuro para "?"
  colores_caracter["-"] <- "red"      # Rojo para "-"
  
  # Graficar el árbol
  plot(filogenia, main = "Árbol Filogenético con Caracteres Múltiples")
  
  # Pintar puntos en cada terminal
  for (i in seq_len(nrow(matriz_datos))) { # Iterar por cada terminal (fila)
    for (j in seq_len(ncol(matriz_datos))) { # Iterar por cada carácter (columna)
      estado <- as.character(matriz_datos[i, j])
      color <- colores_caracter[estado]
      
      if (is.na(color)) color <- "lightgray" # Color por defecto para estados no reconocidos
      
      tiplabels(
        pch = 19, 
        col = color, 
        cex = 1, 
        offset = (j - 2) * 0.05, # Ajustar posición de los puntos
        tip = i
      )
    }
  }
  
  # Generar la leyenda al lado derecho
  leyenda_caracteres <- names(colores_caracter)
  leyenda_colores <- unname(colores_caracter)
  legend(
    "right", 
    legend = leyenda_caracteres, 
    fill = leyenda_colores, 
    cex = 0.8, 
    bty = "n", 
    title = "Caracteres y Colores"
  )
}


arbol <- rtree(3)

# Matriz de datos como entrada
matriz_datos <- matrix(
  c(
    "-", "?", "?", "D", "E", "F", "-",
    "A", "B", "C", "-", "E", "F", "H",
   
    "A", "B", "C", "D", "E", "F", "H"
  ),
  nrow = 3, byrow = TRUE
)

# Ejecutar la función
mapear_multicaracter_unicos(arbol, matriz_datos)



# Ejemplo para un caracter
# Crear un árbol filogenético aleatorio con 10 especies
arbol <- rtree(10)

# Crear una base de datos de caracteres discretos para las especies
datos_especies <- data.frame(
  species = arbol$tip.label, # Usar los nombres de las especies del árbol
  character1 = c("A", "B", "A", "B", "?", "B", "A", "B", "A", "-"),
  character2 = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2)
)

# Ver la base de datos de caracteres
print(datos_especies)

# Ejecutar la función
mapear(arbol, datos_especies)