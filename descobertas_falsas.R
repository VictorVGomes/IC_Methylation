### uma simulação será feita, de forma reprodutível, para determinar a distribuição dos p-valores de testes de hipótese
### (neste caso, o teste t), sob a hipótese nula nos casos em que h_0 é verdade e no caso em que não é.
### 
### O teste é para checar se a média da população (estimada por uma amostra de tamanho n) é igual a 0.
### 
### H_0: \mu = 0
### H_1: \mu \ne 0
###  

test_dist = function(
    n=1000,
    media_testada=0,
    replicate_times=1000,
    seed, ...){
  set.seed(seed)
  t.t = function(){
    amostra = rnorm(n)
    media.amostra = mean(amostra)
    var.amostra = var(amostra)
    var.media.amostra = var.amostra / n
    sd.media.amostra = sqrt(var.media.amostra)
    
    teste_t = abs((media.amostra - media_testada) / sd.media.amostra)
    
    p.valor.teste = 1 - pt(teste_t, df=n-1)
    
    return(c(teste_t, p.valor.teste))
  }
  result = t(replicate(
    replicate_times, t.t(),
  ))
  
  colnames(result) = c('estatistica', 'p.val')
  
  return(result)
}




h0.verdadeiro = test_dist(
  n=10,
  media_testada=0,
  replicate_times = 10000,
  seed=1,
)

hist(h0.verdadeiro[, 2], xlab = 'p-valor', ylab = 'frequência', main='Distribuição do p-valor com h0 verdadeiro')

mean(h0.verdadeiro[, 2] < 0.025) 


h0.falso = test_dist(
  n=10,
  media_testada=0.3,
  replicate_times = 10000,
  seed=1,
)


hist(h0.falso[, 2], xlab = 'p-valor', ylab = 'frequência', main='Distribuição do p-valor com h0 falso')


mean(h0.falso[, 2] < 0.025)

