
# Carregando os pacotes para análise --------------------------------------

library(tidyverse)
library(janitor)
library(rio)
library(openxlsx)

# Definindo a pasta de trabalho -------------------------------------------

setwd("D:/opasR")

getwd()

# Importando as bases de dados --------------------------------------------

dengue_pr <- import("Opasr/dengue_pr_2024.csv")

pop_pr <- import("DADOS/populacao_pr_compactado.zip")

# Inspecionando as bases de dados --------------------------------------------------

# Modificando os nomes das variáveis

# Transforma os nomes para minúsculo, sem acentos com a função janitor::clean_names()

dengue_pr <- clean_names(dengue_pr)

# Renomeando uma coluna com a função dplyr::rename()

dengue_pr <- rename(dengue_pr, "municipio_residencia" = munic_pio_de_resid_ncia)


# Transformação das bases de dados ----------------------------------------

# Transformando a base de dados de DENGUE

# Separando o código e o nome do município em 2 colunas

dengue_pr <- dengue_pr |>
  separate(municipio_residencia, # escolhendo a coluna
           into = c("codigo_municipio", "nome_municipio"), # nomeando as novas colunas
           sep = 6) # definindo onde será a separação

# Corrigindo o caracter de espaço " " excedente
# Usando a notação "$" do R básico

dengue_pr$nome_municipio <- str_trim(dengue_pr$nome_municipio)

# Criando uma nova base de dados com o total de casos em 2024, por município

dengue_pr_2024 <- dengue_pr |>
  select(codigo_municipio, nome_municipio, total)


# Transformando a base de dados de população

# Separando o código e o digito verificador do município em 2 colunas

pop_pr <- pop_pr |>
  separate(codigo, # escolhendo a coluna
           into = c("codigo_municipio", "digito"), # nomeando as novas colunas
           sep = 6) # definindo onde será a separação

# Transformando a base de dados de DENGUE, tornando a semana como variável
# Utilizando a função pivot_longer()

dengue_sem_epi <- dengue_pr |>                               # base de dados e pipe
  select(-total) |>                          # exclui a coluna total
  filter(codigo_municipio != "Total") |>     # exclui a linha Total
  pivot_longer(cols = starts_with("semana"), # seleciona as colunas das semanas
               names_to = "sem_epi",         # nomeia a nova coluna de semanas
               values_to = "casos")          # nomeia a coluna com os casos

# Modificando o conteúdo das colunas, com as funções mutate() e str_replace_all()

dengue_sem_epi <- dengue_sem_epi |>                           # base de dados e pipe
  mutate(sem_epi = str_replace_all(sem_epi,   # mutate para recriar a coluna 
                                   "semana_", # padrão que será substituído
                                   "SE_" ),   # novo padrão
         casos = str_replace_all(casos,       # mutate para recriar a coluna
                                 "-",         # padrão que será substituído
                                 "0"))        # novo padrão

# Transformando a coluna casos em variável do tipo numérica

glimpse(dengue_sem_epi) # verificando o tipo de variável

dengue_sem_epi <- dengue_sem_epi |>                           # base de dados e pipe
  mutate(casos = as.numeric(casos))           # mutate para recriar a coluna
# função as.numeric() para numérico

glimpse(dengue_sem_epi) # verificando o tipo de variável, após a transformação


# Análise da taxa de incidência -------------------------------------------

# Criar uma nova tabela de população apenas com o código e total de habitantes

populacao_parana <- select(pop_pr,                         # base de dados
                           codigo_municipio, pop_total)    # colunas selecionadas

# Criar uma nova tabela de casos, sem os totais

casos_dengue_parana <- dengue_pr_2024 |>                   # base de dados e pipe
  filter(codigo_municipio != "Total") # exclui a linha Total

# Juntando as tabelas para o cálculo da incidência por 100.000 habitantes

incidencia_dengue_pr <- left_join(casos_dengue_parana,     # base de dados
                                  populacao_parana,        # dados que se pretende adicionar
                                  by = "codigo_municipio") # chave de relacionamento

# Opção quando as variáveis não possuem o mesmo nome em tabelas diferentes

incidencia_dengue_pr <- left_join(casos_dengue_parana,
                                  populacao_parana,
                                  by = c("codigo_municipio" = "codigo_municipio"))

# O argumento by = c() pode receber variáveis de nomes diferentes ou mais de uma variável

# Cálculo da incidência usando a função mutate()

incidencia_dengue_pr <- incidencia_dengue_pr |> # base de dados e pipe
  # função round() para arredondar em 2 casas decimais
  mutate(taxa_incidencia = round((total/pop_total)*100000, 2),
         # função if_else para definir alerta em taxa >= a 300/100khab       
         alerta = if_else(taxa_incidencia >= 300, "sim", "não"))


# Exportando as bases de dados em formato *.xlsx --------------------------

# Usando o pacote {openxlsx}
# Vamos criar 3 bases de dados para exportar em uma planilha com 3 abas

# Primeiro arquivo já está pronto, será o data.frame dengue_pr_2024

# Segundo arquivo será a base de dados com as taxas de incidência calculada,
# mas sem a coluna alerta

incidencia_dengue_exportacao <- incidencia_dengue_pr |>
  select(-alerta)

# Terceiro arquivo será a base de dados apenas com os muncípios em alerta

alerta_dengue_exportacao <- incidencia_dengue_pr |>
  filter(alerta == "sim")

# Criar várias planilhas com  pacote {openxlsx}


wb <- createWorkbook()
addWorksheet(wb, sheetName = "Casos prováveis de Dengue 2024")
addWorksheet(wb, sheetName = "Incidência Dengue - PR - 2024")
addWorksheet(wb, sheetName = "Municípios em Alerta - 2024")
writeData(wb, sheet = "Casos prováveis de Dengue 2024", x = dengue_pr_2024)
writeData(wb, sheet = "Incidência Dengue - PR - 2024", x = incidencia_dengue_exportacao)
writeData(wb, sheet = "Municípios em Alerta - 2024", x = alerta_dengue_exportacao)
saveWorkbook(wb,file = "Resumo_Dengue_PR_2024.xlsx")