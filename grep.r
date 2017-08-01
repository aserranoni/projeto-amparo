library(readxl)
#leitura da tabela
dados <- read_excel("C:/Users/Wilson/Desktop/Neuromat - jogo do goleiro/grep/dados.xlsx")
#extracao das arvores
View(dados)
v<-unique(dados$tree)
#vetor de contextos possiveis
poscont<-c("0","1","2","00","01","02","10","20","11","12","21",
      "22","000","001","002","010","020","100","200","011",
      "021","101","201","012","022","102","202","110","210",
      "120","220","111","211","121","221","112","212","122","222")
probs<-matrix(nrow=length(poscont),ncol=50)
#funcao para pegar os padroes
nums2<- list()
nums3<- cnt


for(i in 1:length(arv) + 1){
  
        nums <-grep("([0-999]+):([0-999]+);([0-999])+", arv[i], value=TRUE)
        nums2[[1]] <-strsplit(nums,"[|]")
        for(j in 1: length(nums2)){
          nums3[[i]][j] <- nums2[[1]][j]
        }
}
    
