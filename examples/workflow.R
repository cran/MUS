# diretorio inicial
#setwd("/home/ALSOU/MUS")
#setwd("/projetos/MUS")

rm(list=ls())
origem <- "/home/ALSOU/MUS/examples"
origemWindows <- "/projetos/MUS/examples"
if ((!file.exists(origem)) & file.exists(origemWindows)) {
  origem <- origemWindows
}
origem <- paste0(origem, "/")

file.copy(c(paste0(origem, "example.R")), getwd())
# carrega os data frames
H = 3  # qtd de estratos
sdados = data.frame("stratum"=1:H,
	"conf_level"=rep(conf_level, H),
	"pct_tolerable"=rep(0.1, H),
	"pct_expected"=rep(0.05, H))

# planeja a amostra
inclui_total <- FALSE
MUS.step <- 1
print(sdados)
source("example.R")
# n para cada estrato em sdados$sizes
print(sdados)

# seleciona amostra
MUS.step <- 2
source("example.R")
# selecionados em selected
print(selected)

# avalia
MUS.step <- 3
source("example.R")

# gera pdf
MUS.step <- 4
file.copy(c(paste0(origem, "example.Rmd"), paste0(origem, "logo.png")), getwd())
rmarkdown::render(input="example.Rmd", 
	output_format="pdf_document", 
	output_file='work.pdf', 
	output_options=list()
)
unlink(c("example.Rmd", "logo.png"))

# inclui attachments no pdf
MUS.step <- 5
source("example.R")

# cleanup
unlink(c("report.pdf", "example.R"))
