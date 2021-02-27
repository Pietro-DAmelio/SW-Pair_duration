# SW-Pair_duration

#Data and code for the article:

Influence of pair duration on reproduction under different nest predation pressures in a life-long monogamous cooperative passerine

by:
D’Amelio B. Pietro, Covas Rita, Ferreira C. André, Fortuna Rita, Silva R. Liliana, Theron Franck, Rybak Fanny, Doutrelant Claire







# these info are common to all the scripts

> sessionInfo()
R version 3.5.3 (2019-03-11)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19042)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] ggplot2_3.2.1 sjPlot_2.8.5  loo_2.1.0     brms_2.9.0    Rcpp_1.0.2   

loaded via a namespace (and not attached):
  [1] nlme_3.1-137          matrixStats_0.54.0    xts_0.11-2           
  [4] insight_0.9.6         threejs_0.3.1         rstan_2.19.2         
  [7] tools_3.5.3           backports_1.1.4       R6_2.4.0             
 [10] DT_0.8                sjlabelled_1.1.7      lazyeval_0.2.2       
 [13] colorspace_1.4-1      withr_2.1.2           tidyselect_1.1.0     
 [16] gridExtra_2.3         prettyunits_1.0.2     processx_3.4.1       
 [19] Brobdingnag_1.2-6     emmeans_1.4.7         compiler_3.5.3       
 [22] performance_0.5.0     cli_1.1.0             shinyjs_1.0          
 [25] sandwich_2.5-1        colourpicker_1.0      bayestestR_0.7.2     
 [28] scales_1.0.0          dygraphs_1.1.1.6      mvtnorm_1.0-11       
 [31] ggridges_0.5.1        callr_3.3.1           stringr_1.4.0        
 [34] digest_0.6.20         StanHeaders_2.18.1-10 minqa_1.2.4          
 [37] base64enc_0.1-3       pkgconfig_2.0.2       htmltools_0.3.6      
 [40] lme4_1.1-23           htmlwidgets_1.3       rlang_0.4.7          
 [43] rstudioapi_0.10       shiny_1.3.2           generics_0.0.2       
 [46] zoo_1.8-6             crosstalk_1.0.0       gtools_3.8.1         
 [49] dplyr_0.8.3           inline_0.3.15         magrittr_1.5         
 [52] bayesplot_1.7.0       parameters_0.8.6      Matrix_1.2-15        
 [55] munsell_0.5.0         abind_1.4-5           lifecycle_0.2.0      
 [58] multcomp_1.4-12       stringi_1.4.3         MASS_7.3-51.1        
 [61] pkgbuild_1.0.4        plyr_1.8.4            grid_3.5.3           
 [64] parallel_3.5.3        promises_1.0.1        sjmisc_2.8.5         
 [67] crayon_1.3.4          miniUI_0.1.1.1        lattice_0.20-38      
 [70] splines_3.5.3         ggeffects_0.16.0      sjstats_0.18.0       
 [73] knitr_1.24            ps_1.3.0              pillar_1.4.2         
 [76] igraph_1.2.4.1        boot_1.3-20           estimability_1.3     
 [79] markdown_1.1          shinystan_2.5.0       effectsize_0.3.3     
 [82] codetools_0.2-16      reshape2_1.4.3        stats4_3.5.3         
 [85] rstantools_1.5.1      glue_1.3.1            modelr_0.1.5         
 [88] nloptr_1.2.1          vctrs_0.3.4           httpuv_1.5.1         
 [91] gtable_0.3.0          purrr_0.3.2           tidyr_1.1.2          
 [94] assertthat_0.2.1      xfun_0.9              mime_0.7             
 [97] xtable_1.8-4          broom_0.5.2           coda_0.19-3          
[100] later_0.8.0           survival_3.1-8        rsconnect_0.8.15     
[103] tibble_2.1.3          shinythemes_1.1.2     statmod_1.4.33       
[106] TH.data_1.0-10        bridgesampling_0.7-2 
