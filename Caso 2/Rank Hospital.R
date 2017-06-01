setwd("/Users/amjpc/Documents/HOSPITALES")
rankhospital<-function(estado,resultado,numero){
    datos<-read.csv("outcome-of-care-measures.csv")
    estados<-levels(datos$State)
    
    
    #IF PARA EL PROBLEMA DE SALUD
    if(resultado=="ataque"){
        col<-11
    } else if(resultado=="falla"){
        col<-17
    } else if(resultado=="neumonia"){
        col<-23
    }else {
        stop("¡RESULTADO INVALIDO!")
    }
	
    #IF PARA LOS ESTADOS
    if (estado %in% estados==F){
        stop("¡ESTADO INVALIDO!")
    }

 vectorhos<-c()
 vectornum<-c()
    for(i in 1:nrow(datos)){
     if (datos[i,7]==estado){
         if(datos[i,col]!="Not Available"){
             vehos<-as.character(datos[i,2])
             venum<-as.numeric(as.character(datos[i,col]))
             vectorhos<-c(vectorhos,vehos)
             vectornum<-c(vectornum,venum)
         }
     }
    }
 hospital<-vectorhos
 tasa<-vectornum

tabla<-data.frame(hospital,tasa)
final<-order(tabla$tasa,tabla$hospital)

    #IF's PARA NUMERO
    if (numero=="mejor"){
        numero=1
    }else if (numero=="peor"){
        numero=length(final)
    }
    
    if (numero>length(final)){
        resultado<-NA
    }else{
        resultado<-tabla[final[numero],]
    }
print(resultado)
}
rankhospital("TX","falla",4)