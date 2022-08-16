#Code chapitre 3 sur l'inférence statistique

#Estimation de la copule de Gumbel de paramètre 3 
#via la méthode du maximum de vraisemblance

## The "unknown" copula (a 2-dim. Clayton copula with parameter 3)
Gumbel <- gumbelCopula(2)
#On prendre comme distributions la loi normale N(0,1) et l'exponentielle Exp(1)
mcc <- mvdc(Gumbel, margins = c("norm", "exp"),
            paramMargins = list(list(mean = 0, sd = 1),
                                list(rate = 1)))
## Generate the "observed" sample
set.seed(712)
n <- 1000
X <- rMvdc(n, mvdc = mcc)
## The function fitMvdc() estimates all the parameters of the mvdc object
## mcc (whose parameter values are not used). Starting values need to be
## provided.
start <- c(mu0 = mean(X[,1]), sig0 = sd(X[,1]), lam0 = 1 / mean(X[,2]),
           th0 = 2)
(mle <- fitMvdc(X, mvdc = mcc, start = start))



#Code chapitre 4

#Chargement des packages
library(copula)

#Importation de la base de données
data(loss)

#Nuage de point des données
n <- nrow(loss)
plot(log(loss$loss), log(loss$alae),pch=20,xlab="log des paiements d'indemnités", 
     ylab="log des provisions ajustées pour sinistres survenus",main="Données LOSS-ALAE")

#Diagramme de Kendall
lcopula::K.plot(loss[,1:2],pty="s")

#Calcul des mesures de concordance
#Tau de Kendall
tau <- pcaPP::cor.fk(loss[,1:2])[1,2] 
#Rho de Spearman
rho <- cor(loss[,1:2], method="spearman")[1,2]

#Modèle de copule 
models = c("gumbel", "laplace", "logis", "norm", "exp", "gamma", 
           "invgamma", "invgauss", "invweibull", "llogis", "lnorm", 
           "rayleigh", "weibull", "lgamma", "pareto", "beta", "kumar",
           "logitnorm")


#Transformation des marges en intervalle unitaire
margin_fits <- lapply(data, model_select, models = models, criterion = "aic")

copulas = list(normal = copula::normalCopula(dim = 4, dispstr = "un"),
               frank = copula::frankCopula(dim = 4),
               clayton = copula::claytonCopula(dim = 4),
               gumbel = copula::gumbelCopula(dim = 4))

fits = sapply(copulas,
              function(x) copula::fitCopula(x, data = y, method = "mpl"))

sapply(fits, AIC)