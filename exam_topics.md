# Exam CompStat: 26.7.17 16:00
* yi, y^hat RV
* < y > = mu_y not random -> Prior

## Basic Prop. pf RVs
* Transitional, cond.prob., joint prob. margin


    int(p(x,y),y)=f(x)

* law of propagation errors


    sig^2__y = sig^2_x*(dy/dx)^2

    P(x|y) = P(x,y)/P(y)

## Distributions
* unif., binom., poisson., (multiv.-) gaussian, chi^2, exponential


    f(x) > 0

    int(f(x), x) = 1

* PDF


    f(x)dx

* CDF


    int(f(x), x, -Inf, y) = g(y)

* MGF


    < e^(tx) > = int(e^(tx)* f(x), x)

* moments


    int(x^m*f(x), x)

    int((x-alpha)^m*f(x), x)

## Bayes Theorem

* <>


    P(d, t)

    G(x, mu) = exp(-.5*(x-mu)^2/sig^2)

    P(D|T) = P(D,T)/P(T)

    P(T|D) = P(D,T)/P(D)

* frequentist


    P(D,T) = ...

    d_i, d_hat = sum(di, i)/N = f of data

    < d_hat > = mu

    -> PDF(theta_hat) (PDF of estimator)

* Bayes


    P(T|D) = P(D|T) * P(T)/P(D)

    E(D) = int(L(D|T)* pi(T), T)

    B_(AB) = E_A(D)/E_B(D)

## Fisher Estimation

    F_(α, β) = (dell^2 log(L))/(dell  θ_α * dell θ_β) @ maxL = (..)

* we assume gaussian distr. params. around the peak for a found maximum likelihood estimator


    exp(-.5*(x_i-μ_i)* C_(i,j)^(-1)* (x_j-μ_j))

## fitting

    t = f(x) = A_0 + A_1 * f_1(x) + A_2 * f_2(x)

    exp(-.5* sum((d_i-t_i)^2/σ_i^2))
    -> C_(i,j)

    A |_ maxL =G^-1 D, F_(\alpha, \beta) = G
