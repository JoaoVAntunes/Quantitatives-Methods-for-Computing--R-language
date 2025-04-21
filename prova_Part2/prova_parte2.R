# Aluno: João Vitor de Lima Antunes
# Bacharel em Ciência da Computação
# Prova de Métodos Quantitativos para Computação

# Questão 1
iris_data <- read.csv("C:/Users/joaov/Desktop/Workspace/3º ano/Met. Quantitativos/prova_Part2/iris.csv", 
                      header = FALSE)

# Remove a linha que tem os nomes das colunas como dados
iris_data <- iris_data[-1, ]

# Renomeia corretamente as colunas
colnames(iris_data) <- c("sepal_length", "sepal_width", "petal_length", "petal_width", "species")

# Converte colunas numéricas para tipo numeric pois estava dando erro no cálculo da média
iris_data$sepal_length <- as.numeric(iris_data$sepal_length)
iris_data$sepal_width  <- as.numeric(iris_data$sepal_width)
iris_data$petal_length <- as.numeric(iris_data$petal_length)
iris_data$petal_width  <- as.numeric(iris_data$petal_width)


# Questão 2 

contagem <- function() {
  iris_sem_primeira <- iris_data[-1, ]
  return(table(iris_sem_primeira$species))
}

# Questão 3 – Gráfico de barras da contagem

contagem_grafico <- contagem()

#dev.off()  # Fecha gráficos anteriores
windows(width = 7, height = 5)  

barplot(contagem_grafico,
        col = "steelblue",
        main = "Contagem de Espécies",
        xlab = "Espécies",
        ylab = "Frequência",
        border = "black")

grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")


# Questão 4

library(e1071)

atributos <- c("sepal_length", "sepal_width", "petal_length", "petal_width")

# Função auxiliar para avaliar a simetria
avaliar_simetria <- function(media, mediana, moda) {
  if (is.na(media) || is.na(mediana) || is.na(moda)) {
    return("Não foi possível avaliar (valores ausentes)")
  }
  
  if (abs(media - mediana) < 0.01 && abs(mediana - moda) < 0.01) {
    return("simétrica")
  } else if (media < mediana && mediana > moda) {
    return("assimétrica à esquerda (negativa)")
  } else if (media > mediana && mediana > moda) {
    return("assimétrica à direita (positiva)")
  } else {
    return("assimétrica (não clara)")
  }
}

# Loop para calcular as estatísticas de cada atributo
for (atributo in atributos) {
  dados <- iris_data[[atributo]]
  cat("\n---", atributo, "---\n")
  
  media <- mean(dados)
  mediana <- median(dados)
  moda <- as.numeric(names(sort(table(dados), decreasing = TRUE)[1]))
  
  simetria <- avaliar_simetria(media, mediana, moda)
  
  cat("Média:", media, "\n")
  cat("Mediana:", mediana, "\n")
  cat("Moda:", moda, "\n")
  cat("Distribuição:", simetria, "\n")
  
  # Medidas de dispersão
  desvio_absoluto_medio <- mean(abs(dados - media))
  variancia <- var(dados)
  desvio_padrao <- sd(dados)
  
  cat("Desvio Absoluto Médio:", desvio_absoluto_medio, "\n")
  cat("Variância:", variancia, "\n")
  cat("Desvio Padrão:", desvio_padrao, "\n")
  
  # Percentual de dados além de 1 desvio padrão
  fora_um_dp <- sum(dados < (media - desvio_padrao) | dados > (media + desvio_padrao))
  percentual_fora <- fora_um_dp / length(dados) * 100
  cat("Percentual além de 1 desvio padrão:", round(percentual_fora, 2), "%\n")
}


# Questão 5 – Gráficos de caixa para cada atributo

windows(width = 10, height = 8)  # Janela maior

# Layout 2x2
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))  # margens maiores

# Cores suaves
box_col <- "#66CC99"
border_col <- "#336655"

# Gráfico 1
boxplot(iris_data$sepal_length,
        main = "Boxplot de sepal_length",
        col = box_col,
        border = border_col,
        boxwex = 0.5,           # Largura da caixa
        cex.main = 1.5,
        cex.axis = 1.2,
        ylab = "sepal_length",
        horizontal = FALSE)

# Gráfico 2
boxplot(iris_data$sepal_width,
        main = "Boxplot de sepal_width",
        col = box_col,
        border = border_col,
        boxwex = 0.5,
        cex.main = 1.5,
        cex.axis = 1.2,
        ylab = "sepal_width")

# Gráfico 3
boxplot(iris_data$petal_length,
        main = "Boxplot de petal_length",
        col = box_col,
        border = border_col,
        boxwex = 0.5,
        cex.main = 1.5,
        cex.axis = 1.2,
        ylab = "petal_length")

# Gráfico 4
boxplot(iris_data$petal_width,
        main = "Boxplot de petal_width",
        col = box_col,
        border = border_col,
        boxwex = 0.5,
        cex.main = 1.5,
        cex.axis = 1.2,
        ylab = "petal_width")

