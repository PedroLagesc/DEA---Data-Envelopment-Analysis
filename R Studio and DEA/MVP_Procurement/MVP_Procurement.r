# Intalling the modules
install.packages("Benchmarking")
install.packages("readxl")
install.packages("nonparaeff")

# Calling the Libraries => Modules
library(Benchmarking)
library(readxl)
library(nonparaeff)

# Importing the Dataset from directory
mydata <- read_excel("procurement_dataset.xlsx")

# Checking the imported dataset
View (mydata)

# Create a matrix with input data
x <- as.matrix(with(mydata, cbind(in1, in2, in3, in4, in5, in6, in7, in8)))
View(x)

#Matrix with output data
y <- as.matrix(with( mydata, cbind(out1, out2, out3)))
View(y)

## Constant retuns focused in the inputs
eci <- dea(x,y, RTS="crs", ORIENTATION = "in")
## Constant retuns focused in the outputs
eco <- dea(x,y, RTS="crs", ORIENTATION = "out")
## Variable return focused in the inputs (BCC)
evi <- dea(x,y, RTS="vrs", ORIENTATION = "in")
## Variable return focused in the outputs
evo <- dea(x,y, RTS="vrs", ORIENTATION = "out")

resume <- data.frame(crs_i = eci$eff, crs_o = eco$eff, vrs_i = evi$eff, vrs_o = evo$eff,
                     crs_1o = 1/eco$eff, vrs_1o = 1/evo$eff)
View(resume)

writexl::write_xlsx(resume, "~/UFOP/TCC DEA/R Projects/MVP_Procurement/procurement_resume_EFF_table.xlsx")


## Grph Plots

mydata_matrix <- as.matrix(mydata)
View(mydata_matrix)
col_IN1 <- mydata_matrix[, "in7"]
col_IN2 <- mydata_matrix[, "in8"]

# Verifique se os dados de entrada são numéricos
if (!is.numeric(col_IN1)) {
  col_IN1 <- as.numeric(col_IN1)
}
if (!is.numeric(col_IN2)) {
  col_IN2 <- as.numeric(col_IN2)
}

# Execute a função "dea.plot.isoquant()" com os dados corrigidos
dea.plot.isoquant(col_IN1, col_IN2, RTS = "vrs", txt = TRUE)

# Execute a função "dea.plot.isoquant()" com os dados corretamente acessados
dea.plot.isoquant(col_IN1, col_IN2, RTS = "crs", txt = TRUE)

dea.plot.isoquant(mydata_matrix$in1, mydata_matrix$in2, RTS="vrs", txt=T)

# Plotando a Frontier / fronteira para avaliar a possivel eficiencia
# usando os retornos constantes 
dea.plot.frontier(x, y, RTS="crs", txt=T)

# Usando o plot do Frontier para avaliar a eff
# mediante aos retornos variaveis
dea.plot.frontier(x, y, RTS="vrs", txt=T)
