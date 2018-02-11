# Mapeando a ocorrência do vírus Zika

# ***** Esta é a versão 2.0 deste script, atualizado em 23/05/2017 *****
# ***** Esse script pode ser executado nas versões 3.3.1, 3.3.2, 3.3.3 e 3.4.0 da linguagem R *****
# ***** Recomendamos a utilização da versão 3.4.0 da linguagem R *****

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
setwd("~/Dropbox/DSA/BigDataAnalytics-R-Azure/Projetos/Projeto05")
getwd()


# http://portalsaude.saude.gov.br/index.php/o-ministerio/principal/leia-mais-o-ministerio/1234-secretaria-svs/vigilancia-de-a-a-z/microcefalia-svs/22705&catid=1234&Itemid=250
# http://combateaedes.saude.gov.br/pt/situacao-epidemiologica

# Carregando os pacotes
# devtools::install_github("wch/webshot")
library(dplyr)
library(ggplot2)

# Listando os arquivos e gerando uma lista com os respctivos nomes
temp_files <- list.files(pattern = ".csv")
temp_files

# Carregando todos os arquivos em um único objeto
myfiles <- lapply(temp_files, read.csv, stringsAsFactors = FALSE) 

# Resumo dos arquivos
str(myfiles, 1)
lapply(myfiles, names)[1]
lapply(myfiles, head,2)[1:2]

# Organizando o shape dos dados
brazil <- do.call(rbind, myfiles)
View(brazil)
brazil <- brazil %>% 
  mutate(report_date = as.Date(report_date))

# Visualizando o dataset
glimpse(brazil)

# Transformando o dataframe um uma tabela dplyr e removendo as colunas 5 a 7
brazil <- brazil %>% select(-(6:7)) 

# Visualizando as primeiras 20 linhas
brazil %>% slice (1:20) 

# Para cada reporting_date nós temos 5 regiões
brazil %>% filter(location_type == "region")
brazil %>% filter(location_type == "region") %>% 
  ggplot(aes(x = report_date, y = value, group = location, color = location)) + 
  geom_line() +  
  geom_point() +
  ggtitle("Casos de Zika por Região do Brasil")


# Separando as regiões e Visualizando os Dados
region <- brazil %>% 
  filter(location_type == "region")

region %>% 
  ggplot(aes(x =location, y = value)) + geom_bar(stat = "identity") +
  ylab("Número de Casos Reportados") + xlab("Region") + 
  ggtitle("Casos de Zika Reportados no Brasil")

region %>% 
  slice(1:length(unique(region$location))) %>% 
  arrange(desc(value)) %>%
  mutate(location = factor(location, levels = location,ordered = TRUE)) %>%
  ggplot(aes(x = location, y = value)) + geom_bar(stat = "identity") +
  ylab("Número de Casos Reportados") + xlab("Region") + 
  ggtitle("Casos de Zika Reportados no Brasil")

# Obtendo localidades únicas
region %>% 
  slice(1:length(unique(region$location)))

# Organziando as localidades únicas por número de casos reportados
region %>% 
  slice(1:length(unique(region$location))) %>% 
  arrange(desc(value))

# Criando variáveis do tipo fator
region %>% 
  slice(1:length(unique(region$location))) %>% 
  arrange(desc(value)) %>%
  mutate(location = factor(location,levels=location,ordered=TRUE)) %>% 
  glimpse()

# Agrupando o Sumarizando
brazil_totals <- brazil %>% filter(location=="Brazil") 
region_totals <- brazil %>% filter(location_type=="region") %>%
  group_by(report_date,location) %>%  
  summarize(tot = sum(value)) 

# Padronizar os dados e remover as sumarizações
regvec <- vector()  
length(regvec) <- nrow(brazil)
for (ii in 1:nrow(brazil)) {
  if (brazil[ii,]$location_type != "region")  {
    regvec[ii] <- newlab
  } else {
    newlab <- brazil[ii,]$location
    regvec[ii] <- newlab
  }
}

# Agregando o vetor de regiões ao dataframe brasil
statedf <- cbind(brazil,regvec)

# Eliminar o sumário de linhas por região e país
statedf <- statedf %>% filter(location != "Brazil") 
statedf <- statedf %>% filter(location_type != "region") 

# Gerar o total por regiões a partir dos dados transformados
statedf %>% group_by(report_date,regvec) %>% 
  summarize(tot=sum(value)) -> totals

# Gerando os mapas de cada estado do Brasil
library(ggmap)
longlat <- geocode(unique(statedf$location)) %>% 
  mutate(loc = unique(statedf$location)) 

# Salvando os geocodes do dataframe statedf e salvando em um novo dataframe chamado formapping
statedf %>% filter(as.character(report_date) == "2016-06-11") %>% 
  group_by(location) %>% summarize(cases = sum(value)) %>% 
  inner_join(longlat, by = c("location" = "loc")) %>% 
  mutate(LatLon = paste(lat, lon, sep = ":")) -> formapping

# Visualizando os dados
head(formapping) 

# Formatando a saída e gerando um movo dataframe chamado long_formapping
num_of_times_to_repeat <- formapping$cases
long_formapping <- formapping[rep(seq_len(nrow(formapping)),
                                  num_of_times_to_repeat),]

# Visualizando os dados
head(long_formapping)

# Instalando o pacote leaflet
install.packages("leaflet")
library(leaflet)

# Gerando o mapa com o dataframe
# Aplique o zoom
leaflet(long_formapping) %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())
        
             


