require (ape)
require (Morphometrics)
require (expm)
require (plyr)
require (plotrix)
require (doMC)
registerDoMC (cores = 6)
require (reshape2)
require (ggplot2)
require (phytools)
require (geiger)
require (mvtnorm)

attach ('../../Databases/Reference.RData')
attach ('../../Databases/ED.RData')
attach ('../../Databases/OneDef/OneDef.RData')
attach ('../../Databases/Tree.RData')
attach ('../../Databases/Aux.RData')
#attach ('../../covTensor/Work/post.vcv.RData')

options(contrasts = c('contr.sum', 'contr.poly'))

source ('../FuncR/MLMeh.R')

plot (Tree [[1]], direction = 'upwards', cex = 0.5)
nodelabels(cex = 0.7)

BW.Def <- 
  alply (110:217, 1,
         MLMem, tree = Tree [[1]], data = OneDef, what = 'local',
         .parallel = TRUE)

BW.ED <- 
  alply (110:217, 1,
         MLMem, tree = Tree [[1]], df = ldply (ED, function (L) L $ ed[, -20]),
         what = 'local', .parallel = TRUE)

Results <- list()

Results $ kz.ed <- 
  aaply (1:length (BW.ED), 1, function (i)
         KrzCor(llply (BW.ED, function (L) L $ B)[[i]],
                llply (BW.ED, function (L) L $ W)[[i]],
                ret.dim = min(nrow (BW.ED[[i]] $ means), 19)))

Results $ kz.def <- 
  aaply (1:length (BW.Def), 1, function (i)
         KrzCor(llply (BW.Def, function (L) L $ B)[[i]],
                llply (BW.Def, function (L) L $ W)[[i]]))

Results $ kz.df <-
  data.frame('node' = 110:217, 'def' = Results $ kz.def,
             'ed' = Results $ kz.ed,
             'sample.size' = laply(BW.Def, function (L) nrow (L $ means)))


### Krzanowskiiiii ED vs DEF
ggplot (Results $ kz.df) +
  geom_point(aes (x = def, y = ed, size = sample.size)) +
  scale_size_continuous(expression(n[taxa]), trans = 'log', breaks = c(10, 100)) +
  geom_abline(intercept = 0, slope = 1) +
  theme_minimal() + xlab('KZ Size + Shape') + ylab('KZ Euclidean')



