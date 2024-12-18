directorio ="D:/UNAM 2022/CONAPE/Posdoctorado_Astyanax/servicio social/Mapping characters"
setwd(directorio)

#librerías necesarias
library(ape)
library(phytools)

#lee los datos para correr
new_data <- read.csv("especies_y_caracteres.csv")
ptero_cropped <- read.tree("pterosaurios_mapeo.tre")

#ordeno las terminales del arbol con los caraceres de cada uno
vector_orden <- ptero_cropped$tip.label #creamos un vector que siga el orden del arbol
new_data_ord <- new_data[match(vector_orden, new_data$Species),] #hacemos que el caracter ambiente se ordenen siguiendo el vector previamente creado
identical(ptero_cropped$tip.label, new_data_ord$Species)

#determina los colores que se usarán
col <- c("dodgerblue4", "darkorange2")
tip_colors <- col[new_data_ord$Paleoenvironment] #lo usaremos más adelante

#grafica
plot(
  ptero_cropped,
  edge.width = 3,
  label.offset = 0.05,
  tip.color = tip_colors,
  use.edge.length = FALSE,
  cex = 0.5,
  edge.color = tip_colors, no.margin= TRUE
)


nodelabels()
edgelabels()

length(ptero_cropped$edge)
#428


# Asigna colores a las hojas según su estado

# Colorea las ramas del árbol basándote en los estados terminales
edge_colors <- rep("gray70", nrow(ptero_cropped$edge))  # primero colorea a todo el árbol con un color que designes. 
for (i in seq_along(ptero_cropped$tip.label)) {
  edge_index <- which(ptero_cropped$edge[, 2] == i)  # Encuentra el índice del borde correspondiente
  edge_colors[edge_index] <- tip_colors[i]  # Asigna el color del estado
}


foo <- function() {
  col <- "green"
  for (i in 1:2)
   axis(i, col = col, col.ticks = col, col.axis = col, las = 1)
  box(lty = "19")
  }



plot(ptero_cropped, 
     edge.color = edge_colors, 
     tip.color = tip_colors, 
     cex = 0.5, 
     label.offset = 0.005,
     edge.width = 2)


legend(x="bottomleft",levels(as.factor(new_data_ord$Paleoenvironment)),pch=22,
       pt.bg=col,pt.cex=1.5, bty="n",cex=0.7, title = "Paleoenvironment")


foo()



#####################################################################
#Aquí hay código que estuve modificando para llegar al resultado de arriba. 
########################################################################



# Función para encontrar todas las ramas que llevan a las terminales
find_edges_to_tips <- function(tree) {
  # Extraer matriz de edges del árbol
  edges <- tree$edge
  
  # Identificar los índices de las terminales (nodos hoja)
  tip_indices <- 1:length(tree$tip.label)
  
  # Crear una lista para almacenar los resultados
  paths_to_tips <- vector("list", length(tip_indices))
  names(paths_to_tips) <- tree$tip.label
  
  # Para cada terminal, encontrar las ramas (edges) que llevan a ella
  for (tip in tip_indices) {
    # Inicializar el nodo actual como el terminal
    current_node <- tip
    path <- c()
    
    # Subir por el árbol hasta la raíz
    while (current_node %in% edges[, 2]) {
      parent_edge <- which(edges[, 2] == current_node)
      path <- c(parent_edge, path)  # Agregar edge al camino
      current_node <- edges[parent_edge, 1]  # Moverse al nodo padre
    }
    
    # Guardar el camino encontrado para esta terminal
    paths_to_tips[[tree$tip.label[tip]]] <- path
  }
  
  return(paths_to_tips)
}



# Carga el paquete ape
library(ape)

# Ejemplo de un árbol filogenético
tree <- rtree(10)  # Crea un árbol aleatorio con 10 terminales

# Simula estados de las terminales (por ejemplo, dos estados categóricos)
set.seed(123)  # Fija una semilla para reproducibilidad
states <- sample(c("A", "B"), size = length(tree$tip.label), replace = TRUE)

# Visualiza los estados de las terminales
names(states) <- tree$tip.label  # Asegúrate de que los nombres coincidan
print(states)

# Define colores para los estados
state_colors <- c("A" = "blue", "B" = "red")
tip_colors <- state_colors[states]  # Asigna colores a las hojas según su estado

# Colorea las ramas del árbol basándote en los estados terminales
edge_colors <- rep("black", nrow(tree$edge))  # Color predeterminado para las ramas
for (i in seq_along(tree$tip.label)) {
  edge_index <- which(tree$edge[, 2] == i)  # Encuentra el índice del borde correspondiente
  edge_colors[edge_index] <- tip_colors[i]  # Asigna el color del estado
}

# Dibuja el árbol con ramas coloreadas
plot(tree, edge.color = edge_colors, tip.color = tip_colors, cex = 1.2)
legend("topright", legend = names(state_colors), col = state_colors, pch = 19, title = "Estados")


























#
install.packages("phyloch")  
library(phyloch)
# phylogentic relationships of bird orders:
data(bird.orders)

## 1. EXAMPLE: three monophyletic clades
## -------------------------------------

# define clades:
clades <- list(1:5, 8:12, 13:23)

# generate edge colors:
mycols <- c("orange", "purple", "blue")
ecol <- edge.color(bird.orders, clades, col = mycols)

# plot tree:
plot(bird.orders, edge.color = ecol,
     no.margin = TRUE, edge.width = 2)

## 2. EXAMPLE: depict patristic distance between two tips
## ------------------------------------------------------

group <- c("Strigiformes", "Piciformes")
ecol <- edge.color(bird.orders, group, col = "red")
tcol <- tip.color(bird.orders, group, col = "red")
plot(bird.orders, edge.color = ecol, tip.color = tcol,
     no.margin = TRUE, edge.width = 2)