# Configuración inicial
directorio <- "C:/Users/jatzi/OneDrive/Documentos/Servicio Social/PhyloMapper"
setwd(directorio)

# Librerías necesarias
library(ape)
library(phytools)

# Carga de datos
new_data <- read.csv("especies_y_caracteres_completo.csv")
ptero_cropped <- read.tree("pterosaurios_mapeo.tre")

# Asegurar el orden de los datos según el árbol
vector_orden <- ptero_cropped$tip.label
new_data_ord <- new_data[match(vector_orden, new_data$Species),]

# Validar consistencia entre nombres
if (any(is.na(new_data_ord$Species))) {
  stop("Algunos nombres de especies no coinciden entre el árbol y los datos. Verifica los nombres.")
}
if (!identical(ptero_cropped$tip.label, new_data_ord$Species)) {
  stop("El orden de las especies no coincide entre los datos y el árbol.")
}

# Columnas a analizar
columnas_a_pintar <- colnames(new_data_ord)[colnames(new_data_ord) != "Species"]

# Paleta de colores para las columnas
colores_datos <- rainbow(length(columnas_a_pintar))

# Crear función para asignar colores según el estado del dato
asignar_color <- function(valor, color_columna) {
  if (is.na(valor) || valor == "") {
    return("darkred") # Dato ausente
  } else if (valor == "?") {
    return("lightgray") # Dato incierto
  } else {
    return(color_columna) # Color asociado al dato
  }
}

# Graficar el árbol con espacio adicional para los símbolos
plot(
  ptero_cropped,
  cex = 0.5,
  label.offset = 0.005 + 0.005 * length(columnas_a_pintar),
  edge.width = 2
)

# Crear colores para las ramas del árbol considerando los primeros 3 puntos
edge_colors <- matrix("gray70", nrow = nrow(ptero_cropped$edge), ncol = 3)

# Asignar colores a las ramas basado en los primeros 3 puntos
for (i in seq_along(ptero_cropped$tip.label)) {
  colores_puntas <- c()
  for (j in seq_len(min(3, length(columnas_a_pintar)))) {
    valor <- new_data_ord[[columnas_a_pintar[j]]][i]
    color <- asignar_color(valor, colores_datos[j])
    colores_puntas <- c(colores_puntas, color)
  }
  
  edge_index <- which(ptero_cropped$edge[, 2] == i)  # Encuentra el índice del borde correspondiente
  for (k in seq_along(colores_puntas)) {
    edge_colors[edge_index, k] <- colores_puntas[k]  # Asigna cada color al segmento correspondiente
  }
}

# Dibujar el árbol con las ramas coloreadas en diferentes segmentos
for (k in 1:3) {
  plot(
    ptero_cropped,
    edge.color = edge_colors[, k],
    cex = 0.5,
    label.offset = 0.005 + 0.005 * length(columnas_a_pintar),
    edge.width = 2,
    add = (k != 1)
  )
}

# Agregar símbolos en lugar de etiquetas múltiples con desplazamientos horizontales
for (j in seq_along(columnas_a_pintar)) {
  colores_tips <- mapply(asignar_color, new_data_ord[[columnas_a_pintar[j]]], colores_datos[j])
  tiplabels(
    pch = 16,              # Tipo de símbolo
    col = colores_tips,
    offset = j * 0.005,    # Desplazamiento horizontal por columna
    cex = 0.7
  )
}

# Leyenda para los colores por columna
legend(
  "bottomleft",
  legend = columnas_a_pintar,
  pch = 16,
  col = colores_datos,
  pt.cex = 1.2,
  bty = "n",
  cex = 0.8,
  title = "Columnas analizadas"
)

# Leyenda adicional para estados de datos
legend(
  "topright",
  legend = c("?", "-"),
  pch = 16,
  col = c("lightgray", "darkred"),
  pt.cex = 1.2,
  bty = "n",
  cex = 0.8,
  title = "Estados de datos"
)
