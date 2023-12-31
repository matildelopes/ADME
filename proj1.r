library(Ecdat)
library(stats)
library(WRS2)
library(ggplot2)
library(psych)


#Colocar os dados como é pedido no enunciado

BudgetItaly_sem_pfood <- subset(BudgetItaly, select = -pfood)

BudgetItaly_sem_pfood_73 <- BudgetItaly_sem_pfood[BudgetItaly_sem_pfood$year == 73, ]
BudgetItaly_sem_pfood_73


#-------------------------------//--------------------------------

#BudgetItaly sem year
BudgetItaly_Test <- subset(BudgetItaly_sem_pfood_73,select = -c(year))

#Analise estatistica, graficos, valores e correlações
library(psych)
pairs.panels(BudgetItaly_Test,smooth =FALSE,ellipses=FALSE,lm=TRUE)

#Correlações entre todos
plot(BudgetItaly_Test)

#Analise das correlações entre variáveis
library(GGally)
ggpairs(BudgetItaly_Test)

#---------------------------------------Mahalanobis_Distances_Plots--------------------------------------
lambda <- 0.01  # Fator de regularização
cov_matrix_reg <- cov(BudgetItaly_sem_pfood_73) + lambda * diag(ncol(BudgetItaly_sem_pfood_73))
inv_cov_matrix_reg <- solve(cov_matrix_reg)
mahalanobis_dist <- mahalanobis(BudgetItaly_sem_pfood_73, colMeans(BudgetItaly_sem_pfood_73), inv_cov_matrix_reg)

# Plot das distâncias de Mahalanobis
plot(mahalanobis_dist, main = "Mahalanobis Distances plot", ylab = "Mahalanobis Distance", col = "green", pch = 16)

# Adicionar os índices no gráfico
text(1:length(mahalanobis_dist), mahalanobis_dist, labels = rownames(BudgetItaly_sem_pfood_73), pos = 3, offset = 0.5, col = "blue")


#--------------------------------Mahalanobis_Distances_Boxplot---------------------

# Criar o boxplot e salvar os valores dos outliers
bp <- boxplot(mahalanobis_dist, outline = TRUE, main = "Mahalanobis Distances Boxplot", ylab = "Distance", pch = 16)

# Arredondar os valores dos outliers para quatro casas decimais
rounded_outliers <- round(bp$out, 4)

# Adicionar os valores arredondados dos outliers ao gráfico ao lado dos outliers
text(x = rep(1, length(rounded_outliers)), y = bp$out, labels = rounded_outliers, pos = 4, offset = 0.5, col = "blue")


#-----------------------BOXPLOT_DE_TODOS--------------------------------

library(car)
par(mar=c(1,1,1,1))
par(mfrow=c(3,2))
Boxplot(~wfood, data = BudgetItaly_sem_pfood_73, id.n = Inf, main = "wfood")
Boxplot(~whouse, data = BudgetItaly_sem_pfood_73, id.n = Inf, main = "whouse")
Boxplot(~wmisc, data = BudgetItaly_sem_pfood_73, id.n = Inf, main = "wmisc")
Boxplot(~phouse, data = BudgetItaly_sem_pfood_73, id.n = Inf, main = "phouse")
Boxplot(~pmisc, data = BudgetItaly_sem_pfood_73, id.n = Inf, main = "pmisc")
Boxplot(~totexp, data = BudgetItaly_sem_pfood_73, id.n = Inf, main = "totexp")

#-----------------------BOXPLOT_DE_TODOS--------------------------------

#boxplot sem o valor dos outliers
boxplot(BudgetItaly_Test, col = "lightblue", pch = 16, main = "Boxplot for all variables")



#boxplot com o valor dos outliers
boxplot(BudgetItaly_Test, outline = TRUE, 
        main = "Boxplots para cada variável", col = "lightblue")

# Adicionar os valores dos outliers ao lado das bolinhas dos outliers
for (i in 1:ncol(BudgetItaly_Test)) {
  out <- boxplot(BudgetItaly_Test[, i], outline = TRUE, add = TRUE, at = i, col = "lightblue", pch = 16)$out
  
  if (length(out) > 0) {
    text(rep(i, length(out)), out, labels = round(out, 2), pos = 4, offset = 0.5, col = "blue")
  }
}


#---------------------------------------RELAÇÃO ENTRE INCOME E SIZE--------------------------------


#Existe relação entre o income e o size??

correlation <- cor(BudgetItaly_sem_pfood_73$income, BudgetItaly_sem_pfood_73$size)

print(correlation)

# Gráfico de dispersão entre 'income' e 'size'
plot(BudgetItaly_sem_pfood_73$income, BudgetItaly_sem_pfood_73$size, 
     xlab = "Income", ylab = "Size",
     main = "Gráfico de Dispersão de Income vs Size")


#----------------------------------RELAÇÃO ENTRE INCOME E TOTEXP----------------------------------------------


#Existe relação entre o income e o totexp??
correlation <- cor(BudgetItaly_sem_pfood_73$income, BudgetItaly_sem_pfood_73$totexp)

correlation <- cor(BudgetItaly_Test$income, BudgetItaly_Test$totexp)


print(correlation)


# Gráfico de dispersão entre 'income' e 'totexp'
plot(BudgetItaly_sem_pfood_73$income, BudgetItaly_sem_pfood_73$totexp, 
     xlab = "Income", ylab = "Size",
     main = "Gráfico de Dispersão de Income vs Size")

#------------------------------TODOS OS DADOS DO PONTO 1------------------------


# Aplicar várias operações estatísticas a todas as colunas
library(DescTools)
resultados <- sapply(BudgetItaly_sem_pfood_73, function(col) {
  estatisticas <- c(
    média = mean(col, na.rm = TRUE),
    mediana = median(col, na.rm = TRUE),
    var = var(col, na.rm = TRUE),
    mad = mad(col, na.rm = TRUE),
    sd = sd(col, na.rm = TRUE),
    min = min(col, na.rm = TRUE),
    max = max(col, na.rm = TRUE),
    trimmed_mean = mean(col, trim = 0.1), 
    winsorized_mean = mean(Winsorize(col, probs = c(0.1, 0.9)))
  )
  return(estatisticas)
})

resultados_finais <- subset(resultados, select= -c(year, pct))
resultados_finais

#----------------------------//------------------------------------
BudgetItaly_sem_pfood_73

#correlação entre whouse e wfood
correlation <- cor(BudgetItaly_sem_pfood_73$wfood, BudgetItaly_sem_pfood_73$whouse)

correlation


#--------------------------------------PCA_COM_S----------------------------------

summary(BudgetItaly_Test_PCA_S)


BudgetItaly_Test_PCA_S <- prcomp(BudgetItaly_Test,scale = TRUE)
BudgetItaly_Test_PCA_S

BudgetItaly_Test_PCA_S$x

summary(BudgetItaly_Test_PCA_S)

plot(BudgetItaly_Test_PCA_S$x)

boxplot(BudgetItaly_Test_PCA_S)
#--------------------------------------PCA_COM_S_PLOT----------------------------------
par(mfrow=c(1,2))
plot(
  BudgetItaly_Test_PCA_S$x[,1], BudgetItaly_Test_PCA_S$x[,2],
  xlab="PC1", ylab="PC2", type="n",
  xlim=c(min(BudgetItaly_Test_PCA_S$x[,1]), max(BudgetItaly_Test_PCA_S$x[,1])),
  ylim=c(min(BudgetItaly_Test_PCA_S$x[,2]), max(BudgetItaly_Test_PCA_S$x[,2]))
)

text(
  BudgetItaly_Test_PCA_S$x[,1], BudgetItaly_Test_PCA_S$x[,2],
  rownames(BudgetItaly_Test_PCA_S$x), col="blue", cex=1
)

abline(h=mean(BudgetItaly_Test_PCA_S$x[,2]), col="green")
abline(v=mean(BudgetItaly_Test_PCA_S$x[,1]), col="green")
biplot(BudgetItaly_Test_PCA_S)

#-------------------------------PCA_SEM_S--------------------------
BudgetItaly_Test_PCA_C <- prcomp(BudgetItaly_Test, center = TRUE)
BudgetItaly_Test_PCA_C

BudgetItaly_Test_PCA_C$x

summary(BudgetItaly_Test_PCA_C)

plot(BudgetItaly_Test_PCA_C$x)

biplot(BudgetItaly_Test_PCA_C)

boxplot(BudgetItaly_Test_PCA_C)
#-------------------------------PCA_SEM_S_PLOT--------------------------
#windows(width = 5, height = 5)
par(mfrow=c(1,2))
plot(
  BudgetItaly_Test_PCA_C$x[,1], BudgetItaly_Test_PCA_C$x[,2],
  xlab="PC1", ylab="PC2", type="n",
  xlim=c(min(BudgetItaly_Test_PCA_C$x[,1]), max(BudgetItaly_Test_PCA_C$x[,1])),
  ylim=c(min(BudgetItaly_Test_PCA_C$x[,2]), max(BudgetItaly_Test_PCA_C$x[,2]))
)

text(
  BudgetItaly_Test_PCA_C$x[,1], BudgetItaly_Test_PCA_C$x[,2],
  rownames(BudgetItaly_Test_PCA_C$x), col="blue", cex=1
)

abline(h=mean(BudgetItaly_Test_PCA_C$x[,2]), col="green")
abline(v=mean(BudgetItaly_Test_PCA_C$x[,1]), col="green")
biplot(BudgetItaly_Test_PCA_C)

#--------------------------------PCA SEM E COM S PLOT-------------------------------
par(mfrow=c(1,2))
plot(BudgetItaly_Test_PCA_C$x, col = "green", main = "PCA without standardization", pch = 16)
plot(BudgetItaly_Test_PCA_S$x, col = "blue", main = "PCA with standardization", pch = 16)

#--------------------------------PCA_CLASSICO_0.01_SEM_S-------------------------------
BudgetItaly_outlier <- BudgetItaly_Test


BudgetItaly_outlier[1:5, ] <- BudgetItaly_Test[1:5,] * 0.01

BudgetItaly_outlier_PCA_C <- prcomp(BudgetItaly_outlier, center = TRUE)

summary(BudgetItaly_outlier_PCA_C)

plot(BudgetItaly_outlier_PCA_C$x)

biplot(BudgetItaly_outlier_PCA_C)
#-------------------------CLASSICO_SEM_0.01_Plots-----------------------------

par(mfrow=c(1,2))
plot(
  BudgetItaly_outlier_PCA_C$x[,1], BudgetItaly_outlier_PCA_C$x[,2],
  xlab="PC1", ylab="PC2", type="n",
  xlim=c(min(BudgetItaly_outlier_PCA_C$x[,1]), max(BudgetItaly_outlier_PCA_C$x[,1])),
  ylim=c(min(BudgetItaly_outlier_PCA_C$x[,2]), max(BudgetItaly_outlier_PCA_C$x[,2]))
)

text(
  BudgetItaly_outlier_PCA_C$x[,1], BudgetItaly_outlier_PCA_C$x[,2],
  rownames(BudgetItaly_outlier_PCA_C$x), col="blue", cex=1
)

abline(h=mean(BudgetItaly_outlier_PCA_C$x[,2]), col="green")
abline(v=mean(BudgetItaly_outlier_PCA_C$x[,1]), col="green")
biplot(BudgetItaly_outlier_PCA_C)


#-------------------------PCA_ROBUSTO_0.01_SEM_S-----------------------------

library(rrcov)
# Calcular o PCA Robusto
BudgetItaly_outlier_PCA_C_mcd <- PcaCov(BudgetItaly_outlier, center = TRUE)

# Visualizar os resultados do PCA robusto
summary(BudgetItaly_outlier_PCA_C_mcd)

BudgetItaly_outlier_PCA_C_mcd$scores

plot(BudgetItaly_outlier_PCA_C_mcd$scores)
#-------------------------ROBUSTO_SEM_0.01_Plots-----------------------------

par(mfrow=c(1,2))
plot(
  BudgetItaly_outlier_PCA_C_mcd$scores[,1], BudgetItaly_outlier_PCA_C_mcd$scores[,2],
  xlab="PC1", ylab="PC2", type="n",
  xlim=c(min(BudgetItaly_outlier_PCA_C_mcd$scores[,1]), max(BudgetItaly_outlier_PCA_C_mcd$scores[,1])),
  ylim=c(min(BudgetItaly_outlier_PCA_C_mcd$scores[,2]), max(BudgetItaly_outlier_PCA_C_mcd$scores[,2]))
)

text(
  BudgetItaly_outlier_PCA_C_mcd$scores[,1], BudgetItaly_outlier_PCA_C_mcd$scores[,2],
  rownames(BudgetItaly_outlier_PCA_C_mcd$scores), col="blue", cex=1
)

abline(h=mean(BudgetItaly_outlier_PCA_C_mcd$scores[,2]), col="green")
abline(v=mean(BudgetItaly_outlier_PCA_C_mcd$scores[,1]), col="green")
biplot(BudgetItaly_outlier_PCA_C_mcd)


#-------------------------CLASSICO_E_ROBUSTO_PLOT_0.01-----------------------------

par(mfrow=c(1,2))
plot(BudgetItaly_outlier_PCA_C_mcd$scores, col = "red", main = "Robust PCA x 0.01",pch = 16)
plot(BudgetItaly_outlier_PCA_C$x, col = "blue", main = "Classic PCA x 0.01", pch = 16)

#-------------------------PCA_ROBUSTO_SEM_S-----------------------------

library(rrcov)
# Calcular o PCA Robusto
BudgetItaly_Test_PCA_C_mcd <- PcaCov(BudgetItaly_Test, center = TRUE)

# Visualizar os resultados do PCA robusto
summary(BudgetItaly_Test_PCA_C_mcd)

BudgetItaly_Test_PCA_C_mcd$scores

plot(BudgetItaly_Test_PCA_C_mcd$scores)
