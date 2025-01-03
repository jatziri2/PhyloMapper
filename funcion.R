# Definición de la función
ejecutar_filogenia <- function() {
  # Pedir al usuario la paleta de colores
  cat("Ingrese la paleta de colores para la filogenia (ejemplo: 'rainbow', 'heat.colors', etc.): ")
  paleta <- readline()
  
  # Pedir al usuario el número de caracteres a graficar
  cat("¿Desea graficar un carácter o múltiples caracteres? (Ingrese '1' para uno o 'multi' para varios): ")
  opcion <- readline()
  
  if (opcion == "1") {
    # Primer script
    cat("Ejecutando el script para graficar un carácter...\n")
    
    # Configuración inicial
    directorio <- "C:/Users/jatzi/OneDrive/Documentos/Servicio Social/PhyloMapper"
    setwd(directorio)
    library(ape)
    library(phytools)
    
    new_data <- read.csv("especies_y_caracteres_editado.csv")
    ptero_cropped <- read.tree("pterosaurios_mapeo.tre")
    
    vector_orden <- ptero_cropped$tip.label
    new_data_ord <- new_data[match(vector_orden, new_data$Species),]
    
    if (any(is.na(new_data_ord$Species))) {
      stop("Nombres de especies no coinciden.")
    }
    
    special_colors <- c("-" = "darkred", "?" = "lightgray")
    original_colors <- do.call(paleta, list(length(unique(new_data_ord$Paleoenvironment))))
    names(original_colors) <- levels(as.factor(new_data_ord$Paleoenvironment[!(new_data_ord$Paleoenvironment %in% c("-", "?"))]))
    
    tip_colors <- ifelse(new_data_ord$Paleoenvironment %in% names(special_colors),
                         special_colors[new_data_ord$Paleoenvironment],
                         original_colors[new_data_ord$Paleoenvironment])
    
    edge_colors <- rep("gray70", nrow(ptero_cropped$edge))
    assign_edge_colors <- function(tree, tip_colors, edge_colors) {
      n_tips <- Ntip(tree)
      node_order <- rev((n_tips + 1):(n_tips + Nnode(tree)))
      
      for (node in node_order) {
        desc_edges <- which(tree$edge[, 1] == node)
        desc_nodes <- tree$edge[desc_edges, 2]
        
        desc_colors <- sapply(desc_nodes, function(desc) {
          if (desc <= n_tips) {
            return(tip_colors[desc])
          } else {
            edge_index <- which(tree$edge[, 2] == desc)
            return(edge_colors[edge_index])
          }
        })
        
        predominant_color <- names(sort(table(desc_colors), decreasing = TRUE))[1]
        edge_index <- which(tree$edge[, 2] == node)
        edge_colors[edge_index] <- predominant_color
      }
      
      return(edge_colors)
    }
    
    edge_colors <- assign_edge_colors(ptero_cropped, tip_colors, edge_colors)
    
    for (i in 1:length(ptero_cropped$tip.label)) {
      edge_index <- which(ptero_cropped$edge[, 2] == i)
      edge_colors[edge_index] <- tip_colors[i]
    }
    
    plot(ptero_cropped,
         edge.color = edge_colors,
         tip.color = tip_colors,
         cex = 0.5,
         label.offset = 0.005,
         edge.width = 2)
    
    legend_labels <- c(names(original_colors), names(special_colors))
    legend_colors <- c(original_colors, special_colors)
    legend(x = "bottomleft", legend = legend_labels, pch = 22,
           pt.bg = legend_colors, pt.cex = 1.5, bty = "n", cex = 0.7, title = "Paleoenvironment")
    
  } else if (opcion == "multi") {
    # Segundo script
    cat("Ejecutando el script para graficar múltiples caracteres...\n")
    
    directorio <- "C:/Users/jatzi/OneDrive/Documentos/Servicio Social/PhyloMapper"
    setwd(directorio)
    library(ape)
    library(phytools)
    
    new_data <- read.csv("especies_y_caracteres_completo.csv")
    ptero_cropped <- read.tree("pterosaurios_mapeo.tre")
    
    vector_orden <- ptero_cropped$tip.label
    new_data_ord <- new_data[match(vector_orden, new_data$Species),]
    
    if (any(is.na(new_data_ord$Species))) {
      stop("Nombres de especies no coinciden.")
    }
    
    columnas_a_pintar <- colnames(new_data_ord)[colnames(new_data_ord) != "Species"]
    colores_datos <- do.call(paleta, list(length(columnas_a_pintar)))
    
    asignar_color <- function(valor, color_columna) {
      if (is.na(valor) || valor == "") {
        return("darkred")
      } else if (valor == "?") {
        return("lightgray")
      } else {
        return(color_columna)
      }
    }
    
    plot(ptero_cropped,
         cex = 0.5,
         label.offset = 0.005 + 0.005 * length(columnas_a_pintar),
         edge.width = 2)
    
    for (j in seq_along(columnas_a_pintar)) {
      colores_tips <- mapply(asignar_color, new_data_ord[[columnas_a_pintar[j]]], colores_datos[j])
      tiplabels(pch = 16, col = colores_tips, offset = j * 0.005, cex = 0.7)
    }
    
    legend("bottomleft",
           legend = columnas_a_pintar,
           pch = 16,
           col = colores_datos,
           pt.cex = 1.2,
           bty = "n",
           cex = 0.8,
           title = "Columnas analizadas")
    
    legend("topright",
           legend = c("?", "-"),
           pch = 16,
           col = c("lightgray", "darkred"),
           pt.cex = 1.2,
           bty = "n",
           cex = 0.8,
           title = "Estados de datos")
  } else {
    cat("Opción no válida. Por favor, intente de nuevo.\n")
  }
}

# Llamada a la función
ejecutar_filogenia()
