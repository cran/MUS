# execute report

#setwd("/home/ALSOU/MUS")
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
source("examples/example.R")
# n para cada estrato em sdados$sizes
print(sdados)

# seleciona amostra
MUS.step <- 2
source("examples/example.R")
# selecionados em selected
print(selected)

# inicializa variaveis
MUS.step <- 3
# executa script
source("examples/example.R")

# gera pdf
MUS.step <- 4
render("examples/example.Rmd", pdf_document())

# inclui attachments no pdf
sink("examples/diagnostico.txt")
cat("Informações da Sessão\n\n")
print(sessionInfo())
cat("\n\nVersão do R\n\n")
print(version)
sink()
pdftk("examples/example.pdf", "attach_files examples/data.csv examples/example.R examples/diagnostico.txt", "examples/example2.pdf" )

