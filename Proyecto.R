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
    colores_terminales["?"] <- "lightgray"
    colores_terminales["-"] <- "darkgray"
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

# Ejemplo
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