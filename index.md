
# S Mandujano R

# General description

The main propose of this package is to provide different functions to calculate the RAIs for the species captured with cameras-traps in the same or different locations and seasons. The `RAI` package was development with few functions that allow automatic store of tables, statistical results, figures and maps. 

In this vignette I introduce the R package `RAI` developed to estimate relative abundance indices for different species through photo-trapping data. The package allows to comparison of RAI among species or between selected single species. I presents a step-by-step the procedure for estimating the RAI considering three models: 

1) traditional or classical RAI where total data are grouping for each species; 

2) alternative RAI that allows estimating the variation considering the data from each camera; and 

3) as a generalized Poisson-type linear model with offset. 

# Assumptions

## Population abundance (N) 

Census or counting all the individuals in a population is not a common practice for most wildlife species. Instead, usually is counted fraction of individuals (*C* or *n*) and from this the size or abundance (*N*) of the population is statistically inferred. In some cases all individuals are detected (*p* $\equiv$ 1.0) in the sampling units (e.g., strip transects), but in most situations the detection probability is lower (*p* < 1.0). Therefore, the expected counts is: 

\begin{equation}
E(C) = Np.
\end{equation}

When the probability of detection is estimated then the abundance is:

\begin{equation}
\hat{N} =  \frac{C}{\hat{p}}.
\end{equation}

According to this equation, as $\hat{p}$ approaches 1.0, then counts (*C*) is good index of the relative abundance.

## RAI assumptions {-}

The calculation and interpretation of the relative abundance indices is relatively simple: based on the RAI value, is evaluated if a population of some species is relatively more or less abundant in the same site at different times, or among sites. This interpretation is qualitative and frequently subjective (lacks of statistical rigor) but it is reported in many studies as an indirect measure of the abundance of each species. Therefore, to comparison of temporal/spatial relative abundance of the same species:  

\begin{equation}
RA = \frac {C_2} {C_1} = \frac {p_2 N_2} {p_1 N_1}, 
\end{equation}

thus, if RA > 1 then population increased since counts $C_2$ is relatively greater than $C_1$; while if RA < 1 then population decreased from $t_1$ to $t_2$. However, if $p_1$ $\neq$ $p_2$ then *C* is not a good index, and this could be the common case because the detection varied as consequence of several factors.

The main assumptions of the RAIs are: 

1) there is a positive linear relationship between abundance (*N*) and counts (*C*); 
2) the detection probability (*p*) is perfect, *p* $\equiv$ 1.0; and
3) *p* is constant on average over time and space.

Alternative additional assumption is:

4) variation in *p* is small relative to variation in *N*.


```{r NRAI, echo=FALSE, message=FALSE, warning=FALSE, out.width = "75%", fig.align = 'center', fig.cap= "Figure. 1. Hypothetical relationships between the relative abundance index (RAI) and real abundance (N). a) Comparison of two populations of the same species but with linear and non-linear relationships; and b) comparison among three species with different intensity in the relationship between RAI and N. In both examples, at the same RAI value (open point), the N could be different for each population and species (black point), respectively."}
knitr::include_graphics("indices2.jpg")
```

