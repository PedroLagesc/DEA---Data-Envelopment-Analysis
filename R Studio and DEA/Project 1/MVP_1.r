# Algoritmo baseado na documentação :
# Benchmark and Frontier Analysis Using DEA and SFA
# Link de Referência :
# https://cran.r-project.org/web/packages/Benchmarking/index.html

#Usaremos a funcao "DEA" do R Module e seus default codes :
#dea(X, Y, RTS="vrs", ORIENTATION="in", XREF=NULL, YREF=NULL,
##    FRONT.IDX=NULL, SLACK=FALSE, DUAL=FALSE, DIRECT=NULL, param=NULL,
##    TRANSPOSE=FALSE, FAST=FALSE, LP=FALSE, CONTROL=NULL, LPK=NULL)

# X - Matriz de insumos (INPUTS) das Hospital DMUs analisadas 
# Matriz de ordem K x m - sendo  k DMUs e m Inputs

# Y - Matriz dos produtos (OUTPUTS) incluidos na analise. 
# Matriz de ordemm k x n - sendo k DMUs e n Outputs

# RTS :texto ou numeração definindo o modelo DEA a ser estimado/retornos para escala
# 0 - fdh : Free disposability hull - não assumi a convexidade;
# 1 - vrs : Retornos variaveis para escala - convexidade e free disposability
# 2 - drs : Retornos descrescentes para escala - convexidade , down-scaling e free disposability 
# 3 - crs : Retornos constantes para escala - convexidade e free disposability
# 4 - irs : Retornos crescentes para escala - Sendo up-scaling - convexidade e free disposability
# 5 - irs2: Retornos crescentes para escala - Sendo up-scaling - convexidade aditividade e free disposability
# 6 - add : Aditividade - scaling up e down, mas apenas com inteiros
# 7 - fdh+: Combina a "free disposability" com restricoes ou retornos constantes para a escala local
# 8 - vrs+ :Retornos variaveis para a escala - Sem restricoes para o lambda

# ORIENTATION: insumo  "in" (1), produto "out" (2) - Graph de Eficiencia 

# XREF: INPUTS (Insumo) dos DMUs : X

# YREF: OUTPUTS (Produtos) dos DMUs : Y

# FRONT.IDX: Indices dos DMUs a partir das variáveis de INPU e OUTPUT

# SLACK: Calcula a as folgas dos INPUTS/OUTPUTS na etapa analisada

#Instalando os modulos necessarios e os chamando -> Benchmarking | nonparaeff | Read Excel 

# Executando a instalação
install.packages("Benchmarking")
install.packages("readxl")
install.packages("nonparaeff")

# Chamndo as Libraries => Modulos
library(Benchmarking)
library(readxl)
library(nonparaeff)

# Importando os dados em formato Excel
# Necessario apontar o diretorio
mydata <- read_excel("UFOP/TCC DEA/R Projects/MVP 1/DataSetTest_1.xlsx")
# Vamos visualizar o DataSet = Boa pratica a ser usada
View(mydata)

# Vamos montar a matriz com os INPUTS dos DMUs
# A funcao as.matrix converte os dados unidos em matriz
# Ja a funcao cbind une os dados dos vetores / arrays
x <- as.matrix(with(mydata, cbind(IN1,IN2)))

# Vamos montar a matriz com os OUTPUTS
y <- as.matrix( with(mydata, OUT))
# Para mais OUTPUTS utilizaremos - para n OUTPUS utilizaremos a cbind
# y <- as.matrix( with(mydata, cbind(OUT1,OUT2,OUT3...OUTn)))

# Vamos calcular a  eficiencia dos DMUs
# Argumentos : 
# crs = ret. constantes | vrs = retorno variavel
# in = Modelo orientado aos INPUTS | out = Modelo orientado aos OUTPUTS

# Modelo 1 : Retornos constantes direcionados aos INPUTS
eci <- dea(x,y, RTS="crs", ORIENTATION = "in")
# Modelo 2 : Retornos constantes direcionados aos OUTPUTS
eco <- dea(x,y, RTS="crs", ORIENTATION = "out")

# Modelo 3 : Retornos variaveis direcionados aos INPUTS
evi <- dea(x,y, RTS="vrs", ORIENTATION = "in")
# Modelo 4:  Retornos variaveis direcionados aos OUTPUTS
evo <- dea(x,y, RTS="vrs", ORIENTATION = "out")

# Utilizando os dados obtidos para calcular o score de Eficiencia dos DMUS de acordo com os modelos 
resume <- data.frame(crs_i = eci$eff, crs_o = eco$eff, vrs_i = evi$eff, vrs_o = evo$eff,
                  crs_1o = 1/eco$eff, vrs_1o = 1/evo$eff)
View(resume)

# Insights obtidos atraves da analise dos DMUS :

# Os escores de retornos constantes direcionados tanto aos INPUTS e OUTPUTS
# são iguais e equivalentes

## Os escores de eficiÃªncia com a pressuposiÃ§Ã£o de retornos variÃ¡veis sÃ£o maiores
### do que os calculados sobre a orientaÃ§Ã£o de retornos variÃ¡veis.


# Vamos plotar a isoquanta para verificar quais DMUs sao eficientes e quais nao
dea.plot.isoquant(mydata$IN1, mydata$IN2, RTS="vrs", txt=T)

# Plotando a Frontier / fronteira para avaliar a possivel eficiencia
# usando os retornos constantes 
dea.plot.frontier(x, y, RTS="crs", txt=T)

# Usando o plot do Frontier para avaliar a eff
# mediante aos retornos variaveis
dea.plot.frontier(x, y, RTS="vrs", txt=T)


# sbm.tone calcula as slack variables - variaveis de folga
# E os lambdas - obtencao dos poids

report <- sbm.tone(mydata, noutput = 1)
View(report)

# Vamos exportar os resultados para o formato Excel
writexl::write_xlsx(report, "UFOP/TCC DEA/R Projects/MVP 1/report_table.xlsx")

