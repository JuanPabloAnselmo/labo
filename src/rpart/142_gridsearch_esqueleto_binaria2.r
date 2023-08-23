# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(111235, 125697, 100533, 500697, 666879)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  particionar(dataset, division = c(7, 3), agrupa = "clase_binaria1", seed = semilla)
  
  modelo <- rpart("clase_binaria1 ~ .",
                  data = dataset[fold == 1],
                  xval = 0,
                  control = param_basicos
  )
  
  prediccion <- predict(modelo,
                        dataset[fold == 2],
                        type = "prob"
  )
  
  # Calcula la ganancia utilizando la nueva clasificación binaria
  ganancia_test <- dataset[fold == 2, ]
  ganancia_test$clase_predicha <- ifelse(prediccion[, "BAJA+2"] > 0.025, "BAJA+2", "BAJA+1/CONTINUA")
  
  # Calcula la ganancia para la nueva clasificación binaria
  ganancia_test$ganancia <- ifelse(
    ganancia_test$clase_predicha == "BAJA+2" & ganancia_test$clase_binaria1 == "BAJA+2",
    117000,
    ifelse(ganancia_test$clase_predicha == "BAJA+2" & ganancia_test$clase_binaria1 != "BAJA+2", -3000, 0)
  )
  
  ganancia_test_normalizada <- sum(ganancia_test$ganancia) / 0.3
  
  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  ksemillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
    semillas, # paso el vector de semillas
    MoreArgs = list(param_basicos), # aqui paso el segundo parametro
    SIMPLIFY = FALSE,
    mc.cores = 1
  ) # se puede subir a 5 si posee Linux o Mac OS

  ganancia_promedio <- mean(unlist(ganancias))

  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]

# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch_bin.txt"

# Escribo los titulos al archivo donde van a quedar los resultados
# atencion que si ya existe el archivo, esta instruccion LO SOBREESCRIBE,
#  y lo que estaba antes se pierde
# la forma que no suceda lo anterior es con append=TRUE
cat(
  file = archivo_salida,
  sep = "",
  "max_depth", "\t",
  "min_split", "\t",
  "minbucket", "\t",
  "cp", "\t",
  "ganancia_promedio", "\n"
)


# itero por los loops anidados para cada hiperparametro

for (vmax_depth in seq(6,10,1)) {
  for (minbucket in seq(0,500,25)) {
    for (vmin_split in seq(0,1000,250)) {
      for (cp in c(-0.5, -0.3, 1)){
        param_basicos <- list(
          "cp" = cp,
          "minsplit" = vmin_split,
          "minbucket" = minbucket,
          "maxdepth" = vmax_depth
        )
        
        ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
        
        # Formatear la línea a escribir en el archivo
        linea <- paste(vmax_depth, "\t", vmin_split, "\t", minbucket, "\t", cp, "\t", ganancia_promedio, "\n" )
        
        # Escribir la línea en el archivo
        cat(linea, file = archivo_salida, append = TRUE) 
      }
    }
  }
}

print("Termino")