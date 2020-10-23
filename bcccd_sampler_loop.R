source('bcccd_sampler_roulette.R')


abstract_rolls = as.list(NULL)

for (i in 1:1000){
    abstract_rolls[[i]] = rerollAbstracts(abstracts)
    print(i)
} 

save(abstract_rolls, file = 'abstract_rolls.Rda')