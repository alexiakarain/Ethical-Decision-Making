\# Ethical Decision-Making (R)



R code for an ethical decision-making study on \*\*Dispositional Greed (DG)\*\*, \*\*Moral Disengagement (MD)\*\*, \*\*Moral Identity (MIS)\*\*, and \*\*Unethical Behavior (UB)\*\*.



\*\*What it does\*\*

\- Descriptives \& normality checks

\- Spearman correlations

\- \*\*Ordinal regression\*\* (UB ~ DG + MD + MIS)

\- \*\*Mediation\*\* (DG → MD → UB) and \*\*moderation\*\* (MIS, MIS\_Int, MIS\_Symb)

\- Exploratory \*\*moderated mediation\*\*

\- Ranked ANCOVA (if `group` exists)

\- Saves plots to `outputs/figures/`



\*\*Run\*\*

1\. Put your data at `data/raw/Greed.sav`.

2\. In R: `source("code/analysis.R")`.



> Note: `mediation::mediate` isn’t officially designed for `polr` objects; treat those results as exploratory or consider alternative modelling (e.g., binary UB with `glm`).



\*\*Packages\*\*

psych, MASS, Hmisc, haven, mediation, DiagrammeR, ggplot2, dplyr, car, lmtest



\*\*No data is included in this repo.\*\*



