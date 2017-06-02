setwd("/Users/amjpc/Documents/HOSPITALES")
rankingcompleto<-function(resultado,numero="mejor"){
    datos<-read.csv("outcome-of-care-measures.csv")
    estados<-as.vector(levels(datos$State))
    
    
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
    Hospitales<-c()
    for (buscarestados in 1:length(estados)){
        estado<-estados[buscarestados]
        vectorhos<-c()
        vectornum<-c()
        for(i in 1:nrow(datos)){
            if (as.vector(datos[i,7])==estado){
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
    y<-length(final)
    #IF's PARA NUMERO
    if (numero=="mejor"){
        resultado<-tabla[final[1],1]
    }else if (numero=="peor"){
        resultado<-tabla[final[length(final)],1]
    }else if (numero>length(final)){
        resultado<-NA
    } else{
        resultado<-tabla[final[numero],1]
    }  
    juntar<-as.character(resultado)
    Hospitales<-c(Hospitales,juntar)
    }
print(data.frame(Hospitales, Estado=estados))
}
rankingcompleto("falla")

