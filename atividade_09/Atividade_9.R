arqu_venda_remedios <- "Caminho do CSV /EDA_Industrializados_201402.csv"

library(dplyr)


dados <- read.csv2(arqu_venda_remedios, fileEncoding = "latin1")
# Amostrando 5000 linhas aleatórias da base
amostra <- sample_n(amostra, 500)

# Exportar a amostra para um arquivo CSV
#write.csv(amostra, "Nome do Caminho/amostra.csv", row.names = FALSE)


#separando para fazer a tabela
vendas <- amostra[, c("PRINCIPIO_ATIVO", "QTD_VENDIDA")]
vendas <- sample_n(vendas, 30)
vendas <- arrange(vendas, QTD_VENDIDA)


# Calcular amplitude total e número de classes
AT <- max(vendas$QTD_VENDIDA) - min(vendas$QTD_VENDIDA)  
k <- ceiling(1 + 3.3 * log10(n))  # Regra de Sturges -> k deve ser 6
AC <- ceiling(AT / k)  # Amplitude de cada classe

# Criar intervalos corretos
breaks <- seq(from = min(vendas$QTD_VENDIDA), to = max(vendas$QTD_VENDIDA) + AC, by = AC)

# Criar tabela de frequência com intervalos corrigidos
tabela_freq <- vendas %>%
  mutate(CLASS = cut(QTD_VENDIDA, breaks = breaks, include.lowest = TRUE, right = FALSE)) %>%
  group_by(CLASS) %>%
  summarise(fi = n(), .groups = "drop") %>%
  mutate(Fi = cumsum(fi)) %>%
  # Extração correta dos limites
  mutate(
    lim_inf = as.numeric(str_extract(CLASS, "\\d+")),
    lim_sup = as.numeric(str_extract(CLASS, "(?<=,)\\s*\\d+")),
    xi = (lim_inf + lim_sup) / 2  # Calcula o ponto médio
  ) %>%
  select(CLASS, fi, Fi, xi)  # Seleciona colunas na ordem correta

# Exibir tabela corrigida
print(tabela_freq)
