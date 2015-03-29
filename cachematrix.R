## Funcao para criacao de matriz com cache
## This matrix work with CacheSolve. makeCacheMatrix save the matrix on the cache by "<<-"

makeCacheMatrix <- function(matriz_origem = matrix()) {
        
        inv_matriz <- NULL
        
        set <- function(dados) {
                matriz_origem <<- dados
                inv_matriz <<- NULL
        }
        
## Funcao para setar e obter matriz invertida
## This function is to set and get inverted matrix
        get <- function() matriz_origem

        set_inverse <- function(solve) inv_matriz <<- solve
        get_inverse <- function() inv_matriz
        
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## testa se o inverso da matriz foi armazenada, se sim imprime matriz caheada
## test if inversed matrix had storaged in the cache or not. In positive case print the message "Matriz Cacheada"
## in negative case print "Matriz nao estava no cache"

cacheSolve <- function(matriz_no_cache, ...) {
        inv_matriz <- matriz_no_cache$get_inverse()
        
## testa se a matriz esta no cache
        if(!is.null(inv_matriz)) { 
                print("Matriz Cacheada")
                return(inv_matriz)
        }
        
## Matriz nao esta no cache
        matriz <- matriz_no_cache$get()
        inv_matriz <- solve(matriz)
        matriz_no_cache$set_inverse(inv_matriz)
        inv_matriz
        print("Matriz nao estava no cache")
}



## t e s t e    d e      pr o g r a m a ##

## cria matriz para verificacao
matriz_de_teste <- matrix(runif(100,1,10),10,10)
##chama a funcao para fazer o cache
esta_no_cache <- makeCacheMatrix(matriz_de_teste)
## primera vez fora do cache, nas proximas no cache
retorno <- cacheSolve(esta_no_cache)
retorno <- cacheSolve(esta_no_cache)


