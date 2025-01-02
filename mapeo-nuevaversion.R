# Configuración inicial
directorio <- "C:/Users/jatzi/OneDrive/Documentos/Servicio Social/PhyloMapper"
setwd(directorio)

# Librerías necesarias
library(ape)
library(phytools)

# Lee los datos
new_data <- read.csv("especies_y_caracteres_editado.csv")
ptero_cropped <- read.tree("pterosaurios_mapeo.tre")

# Ordena los datos
vector_orden <- ptero_cropped$tip.label
new_data_ord <- new_data[match(vector_orden, new_data$Species),]

# Verifica NAs
if (any(is.na(new_data_ord$Species))) {
  stop("Nombres de especies no coinciden.")
}

# Define colores especiales
special_colors <- c(
  "-" = "darkred",
  "?" = "lightgray"
)

# Colores originales (azul y naranja)
original_colors <- c("dodgerblue4", "darkorange2")
names(original_colors) <- levels(as.factor(new_data_ord$Paleoenvironment[!(new_data_ord$Paleoenvironment %in% c("-", "?"))]))

# Asigna colores a las puntas, incluyendo los colores especiales
tip_colors <- ifelse(new_data_ord$Paleoenvironment %in% names(special_colors),
                     special_colors[new_data_ord$Paleoenvironment],
                     original_colors[new_data_ord$Paleoenvironment])

# Verifica colores asignados
if (any(is.na(tip_colors))) {
  stop("No se pudieron asignar colores.")
}

# Inicializa colores de las ramas
edge_colors <- rep("gray70", nrow(ptero_cropped$edge))

# Función recursiva corregida para colorear las ramas internas
assign_edge_colors <- function(tree, tip_colors, edge_colors) {
  n_tips <- Ntip(tree)
  
  # Orden de nodos: de las hojas a la raíz
  node_order <- rev((n_tips + 1):(n_tips + Nnode(tree)))
  
  for (node in node_order) {
    # Obtiene los descendientes inmediatos (hojas o nodos internos)
    desc_edges <- which(tree$edge[, 1] == node)
    desc_nodes <- tree$edge[desc_edges, 2]
    
    # Colores de los descendientes
    desc_colors <- sapply(desc_nodes, function(desc) {
      if (desc <= n_tips) {
        return(tip_colors[desc])
      } else {
        edge_index <- which(tree$edge[, 2] == desc)
        return(edge_colors[edge_index])
      }
    })
    
    # Color predominante entre los descendientes
    predominant_color <- names(sort(table(desc_colors), decreasing = TRUE))[1]
    
    # Asigna el color predominante a la rama que conecta con este nodo
    edge_index <- which(tree$edge[, 2] == node)
    edge_colors[edge_index] <- predominant_color
  }
  
  return(edge_colors)
}

# Aplica la función para asignar colores a las ramas internas
edge_colors <- assign_edge_colors(ptero_cropped, tip_colors, edge_colors)

# Colorea las ramas terminales
for (i in 1:length(ptero_cropped$tip.label)) {
  edge_index <- which(ptero_cropped$edge[, 2] == i)
  edge_colors[edge_index] <- tip_colors[i]
}

# Grafica el árbol
plot(ptero_cropped,
     edge.color = edge_colors,
     tip.color = tip_colors,
     cex = 0.5,
     label.offset = 0.005,
     edge.width = 2)

# Leyenda para los colores
legend_labels <- c(names(original_colors), names(special_colors))
legend_colors <- c(original_colors, special_colors)
legend(x = "bottomleft", legend = legend_labels, pch = 22,
       pt.bg = legend_colors, pt.cex = 1.5, bty = "n", cex = 0.7, title = "Paleoenvironment")
