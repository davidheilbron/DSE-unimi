set.seed(42)
library(BVAR)

x <- fred_qd[1:243, c("GDPC1", "PCECC96", "GPDIC1", "HOANBS", "GDPCTPI", "FEDFUNDS")]
x <- fred_transform(x, codes = c(4, 4, 4, 4, 4, 1))
str(x)

#minnesota prior
mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.001, max = 5),
  alpha = bv_alpha(mode = 2),
  var = 1e07
)

#sum of coefficients prior
soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)

#single unit root prior
sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)

priors <- bv_priors(hyper = 'auto', mn = mn, soc = soc, sur = sur)

mh <- bv_metropolis(scale_hess = c(0.05, 0.0001, 0.0001),
                    adjust_acc = TRUE, acc_lower = 0.25, acc_upper = 0.45)


run <- bvar(x, lags = 5, n_draw = 15000, n_burn = 5000, n_thin = 1, priors = priors, mh = mh, verbose = TRUE)
