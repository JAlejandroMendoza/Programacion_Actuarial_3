setwd("/Users/amjpc/Documents/HOSPITALES")
mejor<-function(estado,resultado){
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
    
    hospital<-vectorhos
    tasa<-vectornum
    }
    minimo<-min(tasa)
    tabla<-data.frame(hospital,tasa)
    fin<-order(tabla$tasa,tabla$hospital)
    resultado<-tabla[fin[1],]
    print(resultado)

}
mejor("TX", "ataque")