# Criando os dados
erros <- c(0, 1, 2, 3, 4)
frequencia <- c(25, 20, 3, 1, 1)

dev.off()  # Fecha gráficos anteriores
windows(width = 7, height = 5)  # Abre uma nova janela gráfica
barplot(frequencia, names.arg = erros, col = "blue", 
        main = "Distribuição do Número de Erros por Página", 
        xlab = "Número de Erros", ylab = "Frequência", 
        border = "black")


# Adicionando grade no eixo Y
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
