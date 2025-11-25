# 0. Setup
install.packages(c("tidyverse", "lubridate"))
library("tidyverse")
library("lubridate")
library("ggplot2")
library("scales")

# 1. Importe o seu dataset para o R.
df_coffee_shop <- read.csv("Project.csv", sep=',')
df_coffee_shop

# 4. Verifique as primeiras 6 linhas do dataset.
head(df_coffee_shop, 6)

# 5. Verifique as últimas 10 linhas do dataset.
tail(df_coffee_shop, 10)

# 6. Mostre a quantidade de linhas e colunas do dataset.
dim(df_coffee_shop)

# 7. Exiba apenas os nomes das colunas do dataset.
names(df_coffee_shop)

# 9. Verifique o tipo de todas as colunas do dataset e ajuste para o tipo correto
str(df_coffee_shop)
df_coffee_shop$transaction_date <- as.Date.character(df_coffee_shop$transaction_date,"%d-%m-%Y")
df_coffee_shop$transaction_time <- hms(df_coffee_shop$transaction_time)
df_coffee_shop$transaction_id <- as.character(df_coffee_shop$transaction_id)
df_coffee_shop$product_id <- as.character(df_coffee_shop$product_id)
df_coffee_shop$store_id <- as.character(df_coffee_shop$store_id)

# 10. Selecione apenas duas colunas do dataset.
select(df_coffee_shop, product_type, unit_price)

# 11. Filtre as linhas onde uma variável numérica seja maior que um valor definido. 
filter(df_coffee_shop, unit_price > 10)

# 12. Ordene o dataset de forma crescente com base em uma coluna numérica.
arrange(df_coffee_shop, unit_price)

# 13. Crie uma nova coluna com base em uma operação entre duas colunas existentes.
df_coffee_shop <- df_coffee_shop %>% mutate(total_bill = transaction_qty * unit_price)

# 14. Remova uma coluna do dataset.
df_coffee_shop <- df_coffee_shop %>% select(-Total_Bill)
df_coffee_shop

# 15. Use a função select() para escolher 3 colunas do dataset.
select(df_coffee_shop, product_category:product_detail)

# 16. Use a função filter() para selecionar linhas que atendam a uma condição.
filter(df_coffee_shop, unit_price > 10, transaction_qty > 1)

# 17. Selecione todas as colunas cujo nome começa com uma letra específica usando select(starts_with()).
select(df_coffee_shop, starts_with("p"))

# 18. Renomeie duas colunas do dataset usando rename().
df_coffee_shop <- rename(df_coffee_shop, month_name = Month.Name, day_name = Day.Name, day_of_week = Day.of.Week)

# 19. Utilize arrange() para ordenar os dados de forma decrescente.
arrange(df_coffee_shop, desc=transaction_date)

# 20. Crie uma nova coluna com mutate().
df_coffee_shop <- df_coffee_shop %>%
  mutate(mean_bill = mean(total_bill))
df_coffee_shop

# 21. Resuma os dados de uma coluna numérica usando summarise().
df_coffee_shop %>%
  summarize(median_qty = median(transaction_qty))
df_coffee_shop

# 22. Agrupe os dados por uma variável categórica com group_by().
df_coffee_shop %>% 
  group_by(product_category) %>% 
  summarise(sum_bill_by_product_category = sum(total_bill, na.rm=TRUE))

# 23. Combine group_by() e summarise() para calcular a média de uma variável por grupo.
df_coffee_shop %>% 
  group_by(month_name) %>% 
  summarise(mean_bill_by_month_name = mean(total_bill, na.rm=TRUE))

# 24. Use pivot_longer() para transformar colunas em linhas.
pivot <- df_coffee_shop %>%
  select(transaction_id, transaction_date, day_name, product_category) %>% 
  pivot_longer(cols = day_name,
               names_to = "variavel",
               values_to = "valor")
pivot
# 25. Utilize um pipeline para: selecionar colunas, filtrar linhas e ordenar os dados.
df_coffee_shop %>%
  filter(month_name == "June") %>% 
  select(starts_with("p")) %>%
  arrange(product_type)

# 26. Use pivot_wider() para transformar linhas em colunas.
pivot %>% 
  pivot_wider(names_from = variavel,
              values_from = valor)

# 27. Aplique drop_na() para remover valores ausentes.
drop_na(df_coffee_shop)

# 28. Substitua valores ausentes por 0 em uma coluna numérica.
mutate(df_coffee_shop, transaction_qty = replace_na(transaction_qty,0))

# 29. Crie um gráfico de dispersão (scatterplot) com duas variáveis numéricas.
df_count_sales_dia_tea<- df_coffee_shop %>%
  filter(product_category == "Tea") %>% 
  select(Hour, total_bill)

ggplot(data = df_count_sales_dia_tea, aes(x= Hour, y= total_bill)) +
  geom_point(color = "steelblue", alpha=0.6) +
  labs(
    title= "Distribuição do Valor das Vendas do Chá por Hora",
    x= "Hora",
    y= "Valor da Venda ($)") +
  theme_minimal()

# 30. Crie um gráfico de barras de uma variável categórica.
df_coffee_shop %>%
  group_by(product_category) %>%
  summarise(total_bill = sum(total_bill)) %>%
  ggplot(aes(x = reorder(product_category, total_bill),
             y = total_bill, fill =  product_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = dollar(total_bill, accuracy = 1)),
            hjust = -0.1, size = 3) + 
  scale_y_continuous(labels = scales::label_comma()) +
  scale_fill_manual(
    values = c("#deebf7", "#c6dbef", "#9ecae1", "#6baed6",
               "#4292c6", "#2171b5", "#08519c", "#08306b", "#041f47"),
    name = "Tipos de Produtos") +
  labs(title = "Valor Total de Vendas por Tipo de Produto", 
       x = "Produto", 
       y = "Valor Total ($)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 9),        
    legend.title = element_text(size = 10),      
    legend.key.size = unit(0.3, "lines"),       
    plot.margin = margin(10, 30, 10, 10)) +
  coord_flip(clip = "off")
  
# 31. Construa um histograma de uma variável numérica. 
df_coffee_shop %>%
  ggplot(mapping = aes(x = Hour)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Qtd. de Vendas por Hora",
    x = "Hora",
    y = "Qtd. de Vendas"
  ) 

# 32. Crie um gráfico de linha para visualizar a evolução de uma variável ao longo do tempo. 
df_count_sales_data <- df_coffee_shop %>%
  mutate(data = floor_date(transaction_date, "month")) %>% 
  group_by(data) %>% 
  summarise(count_sales = n())

ggplot(df_count_sales_data, aes(x= data, y=count_sales)) + 
  geom_text(aes(label = count_sales), vjust = -0.5, size = 3) +
  geom_line(color = "steelblue", size=1) +
  geom_point(color = "steelblue", size=2) +
  expand_limits(y = max(df_count_sales_data$count_sales) * 1.1) +
  labs(
    title= "Evolução Mensal da Quantidade de Vendas",
    x= "Mês",
    y= "Qtd. de Vendas") +
  theme_minimal()

# 33. Adicione uma linha de tendência a um gráfico de dispersão. 
df_count_sales_dia<- df_coffee_shop %>%
  filter(product_category == "Coffee", Month == 1) %>% 
  select(Hour, total_bill)

ggplot(data = df_count_sales_dia, aes(x= Hour, y= total_bill)) +
  geom_point(color = "steelblue", alpha=0.6) +
  geom_smooth(method= "lm", se = TRUE, color = "red") +
  labs(
    title= "Distribuição do Valor das Vendas do Café por Hora — Janeiro",
    x= "Hora",
    y= "Valor da Venda ($)") +
  theme_minimal()

# 34. Crie um boxplot para comparar a distribuição de uma variável numérica entre categorias.
ggplot(data = df_coffee_shop)+
  geom_boxplot(
    mapping = aes(
      x = reorder(store_location, total_bill, FUN = sum),
      y = total_bill)) + 
  scale_y_log10() +
  labs(
    title= "Distribuição do Total de Vendas por Loja",
    x= "Local da Loja",
    y= "Total da Venda($)") +
  theme_minimal() 

# 35. Personalize um gráfico com título, legenda e rótulos nos eixos.
df_summary <- df_coffee_shop %>%
  group_by(store_location) %>%
  summarise(
    total_sales = n(),
    total_bill = sum(total_bill)
  ) %>%
  pivot_longer(cols = c(total_sales, total_bill),
               names_to = "metric",
               values_to = "value")

ggplot(df_summary, aes(x = reorder(store_location, value), y = value, fill = metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = ifelse(metric == "total_sales", value, dollar(value, accuracy = 1))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(
    values = c("total_sales" = "#4292c6", "total_bill" = "#08519c"),
    labels = c("Receita Total","Qtd. de Vendas"),
    name = "Métricas"
  ) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Qtd. de Vendas e Receita Total por Loja",
    x = "Loja",
    y = "Valores"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    legend.key.size = unit(0.3, "lines"),
    plot.margin = margin(10, 10, 10, 10)
  )

# 36. Crie um mapa de calor (heatmap) com duas variáveis categóricas.
df_heatmap <- df_coffee_shop %>%
  group_by(day_name, Hour) %>%
  summarise(total_sales = sum(total_bill), .groups = "drop")
  
ggplot(df_heatmap, aes(x= Hour, y= day_name, fill = total_sales)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", name = "Vendas ($)") +
  labs(
    title = "Heatmap de Vendas por Hora e Dia da Semana",
    x = "Hora do Dia",
    y = "Dia da Semana"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.key.width = unit(0.6, "lines"),
    legend.key.height = unit(1, "lines")
  )

# 37. Combine mais de um gráfico em uma mesma visualização usando facet_wrap().
df_coffee_shop %>%
  ggplot(mapping = aes(x = Hour)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Qtd. de Vendas por Hora",
    x = "Hora",
    y = "Qtd. de Vendas"
  ) +
  facet_wrap(~store_location)
