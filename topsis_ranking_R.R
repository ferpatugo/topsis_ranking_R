#' @title teste
#'
#' @description teste
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples
#' 
#' 
#' @export

topsis_ranking_R=function(){
  
  tipo_criterios=list()
  consolidado=list()
  consolidado2=list()
  vetor_A_mais=list()
  vetor_A_menos=list()
  d_A_mais=list()
  d_A_menos=list()
  importancias_relativas=list()
  consolidado3=data.frame()
  consolidado_importancias=data.frame()
  vetor_nomes_alternativas_A_mais=list()
  vetor_nomes_alternativas_A_menos=list()
  
  library(reshape)
  library(dplyr)
  
  projeto = readline("Qual o nome do projeto? ")  
  
  alternativas = readline("Quais sao as alternativas? ")
  alternativas = unlist(strsplit(alternativas, ","))
  
  numero_alternativas=length(unlist(strsplit(alternativas, ",")))
  
  criterios = readline("Quais sao os criterios do projeto? ")
  criterios = unlist(strsplit(criterios, ","))
  
  numero_criterios=length(unlist(strsplit(criterios, ",")))
  
  vetor_pesos = readline("Digite o vetor de pesos")
  vetor_pesos = as.numeric(unlist(strsplit(vetor_pesos, ",")))
  
  vetor_valores= readline("Digite o vetor de valores")
  vetor_valores= as.numeric(unlist(strsplit(vetor_valores, ",")))
  
  for (i in 1:numero_criterios){
    
    tipo_criterios[[i]] = readline(as.character(cat("Que tipo do criterio e o criterio" ,criterios[i],": Monotonico de custo:digite mc ou Beneficio: digite mb)?")))
    
  }
  
  dados=matrix(vetor_valores,
               dimnames =list(alternativas,
                              criterios),
               nrow=numero_alternativas,ncol=numero_criterios,byrow = T)
  
  for (i in 1:numero_criterios) {
    
    consolidado[[i]]=dados[,i]/norm(dados[,i], type="2")
    
    consolidado2[[i]]=vetor_pesos[i]*consolidado[[i]]
    
  }
  
  
  for (i in 1:numero_criterios) {
    
    if (tipo_criterios[[i]]=="mb") {
      
      vetor_A_mais[[i]]=max(consolidado2[[i]])
      vetor_nomes_alternativas_A_mais[[i]]=names(consolidado2[[i]])[consolidado2[[i]]==max(consolidado2[[i]])]
      vetor_A_menos[[i]]=min(consolidado2[[i]])
      vetor_nomes_alternativas_A_menos[[i]]=names(consolidado2[[i]])[consolidado2[[i]]==min(consolidado2[[i]])]
      
      
    } else {
      
      vetor_A_mais[[i]]=min(consolidado2[[i]])
      vetor_nomes_alternativas_A_mais[[i]]=names(consolidado2[[i]])[consolidado2[[i]]==min(consolidado2[[i]])]
      vetor_A_menos[[i]]=max(consolidado2[[i]])
      vetor_nomes_alternativas_A_menos[[i]]=names(consolidado2[[i]])[consolidado2[[i]]==max(consolidado2[[i]])]
      
    }
  }
  
  for (i in 1:numero_alternativas) {
    
    consolidado3=data.frame(consolidado2)
    data_vetor_A_mais=data.frame(vetor_A_mais)
    names(consolidado3)=names(data_vetor_A_mais)
    
    d_A_mais[[i]]= dist(rbind(consolidado3[i,],data_vetor_A_mais), method = "euclidean")
  }                
  
  
  for (i in 1:numero_alternativas) {
    
    data_vetor_A_menos=data.frame(vetor_A_menos)
    names(consolidado3)=names(data_vetor_A_menos)
    
    d_A_menos[[i]]= dist(rbind(consolidado3[i,],data_vetor_A_menos), method = "euclidean")
  }                
  
  for (i in 1:numero_alternativas) {
    
    importancias_relativas[[i]]=d_A_menos[[i]]/(d_A_mais[[i]]+d_A_menos[[i]])     
    
    
  }
  
  consolidado_importancias=reshape::melt(as.matrix(importancias_relativas))
  
  consolidado_importancias=consolidado_importancias %>% dplyr::select(value)
  
  consolidado_importancias$value=round(as.numeric(consolidado_importancias$value),5)
  
  consolidado_importancias$Alternativas=alternativas
  
  consolidado_importancias=consolidado_importancias %>% dplyr::arrange(desc(value))
  
  consolidado_importancias$Ranking=seq(1,by=1,dim(consolidado_importancias)[1])
  
  names(consolidado_importancias)=c("Pontuacao de desempenho","Alternativas","Ranking")
  
  assign("matriz de entrada",dados,envir = .GlobalEnv)
  assign("matriz normalizada",consolidado,envir = .GlobalEnv)
  assign("matriz normalizada ponderada",consolidado2,envir = .GlobalEnv)
  assign("Vetor A+",vetor_A_mais,envir = .GlobalEnv)
  assign("Vetor A-",vetor_A_menos,envir = .GlobalEnv)
  assign("Distancia A+",d_A_mais,envir = .GlobalEnv)
  assign("Distancia A-",d_A_menos,envir = .GlobalEnv)
  assign("importancias_relativas",importancias_relativas,envir = .GlobalEnv)
  assign("consolidado_final",consolidado_importancias,envir = .GlobalEnv)
  
  cat("O nome do seu projeto e:",projeto,"\n")
  cat("Aternativas do seu projeto:",alternativas,"\n")
  cat("Criterios do seu projeto:",criterios,"\n")
  cat("O resultado final e:","\n")
  cat("---------------------","\n")
  print(consolidado_importancias)
  cat("---------------------")
}

