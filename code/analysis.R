# Ethical Decision-Making: analysis pipeline ---------------------------------

# Packages --------------------------------------------------------------------
pkgs <- c("psych","MASS","Hmisc","haven","mediation","DiagrammeR",
          "ggplot2","dplyr","car","lmtest")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
invisible(lapply(pkgs, require, character.only = TRUE))

# Paths -----------------------------------------------------------------------
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)

# Data ------------------------------------------------------------------------
# Place your file at data/raw/Greed.sav
stopifnot(file.exists("data/raw/Greed.sav"))
data <- haven::read_sav("data/raw/Greed.sav")

# Codebook-ish selection
vars <- dplyr::select(data, DG, MD, UB, MIS)

# Descriptives ----------------------------------------------------------------
numeric_vars <- dplyr::select(vars, where(is.numeric))
if (ncol(numeric_vars) > 0) {
  print(psych::describe(numeric_vars))
}
# UB summary as provided
suppressWarnings({
  print(summary(vars$UB))
  data$UB <- as.numeric(vars$UB)
  print(median(data$UB, na.rm = TRUE))
  print(IQR(data$UB, na.rm = TRUE))
})

# Normality helper (visual)
normality <- function(variable, name){
  par(mfrow = c(1,2))
  hist(variable, main = paste("Histogram of", name), xlab = name, col = "lightblue")
  qqnorm(variable, main = paste("Q-Q Plot of", name)); qqline(variable, col = "red")
}
invisible(lapply(names(numeric_vars), function(v) normality(numeric_vars[[v]], v)))

# Shapiro tests (note: small-sample only; with large n it always rejects)
shapiro_results <- lapply(numeric_vars, shapiro.test)
print(shapiro_results)

# Spearman correlations
correlation_spearman <- suppressWarnings(cor(dplyr::select(vars, where(is.numeric)),
                                             use = "complete.obs", method = "spearman"))
print(correlation_spearman)
print(Hmisc::rcorr(as.matrix(dplyr::select(vars, where(is.numeric))), type = "spearman")$P)

# Ordinal regression (UB must be ordered factor) ------------------------------
data$UB_ord <- factor(data$UB, ordered = TRUE)

model <- MASS::polr(UB_ord ~ DG + MD + MIS, data = data, Hess = TRUE, method = "logistic")
summary_model <- summary(model)  # define BEFORE using
print(summary_model)

# p-values from |z| ~ N(0,1)
t_values <- coef(summary_model)[, "t value"]
p_values  <- 2 * pnorm(abs(t_values), lower.tail = FALSE)
print(p_values)

# ORs and CI (Wald CI shown; profile CIs can be slow)
odds_ratios <- exp(coef(model))
ci_wald     <- exp(confint.default(model))
print(odds_ratios); print(ci_wald)

# Subscales (if present)
if (all(c("MIS_Int","MIS_Symb") %in% names(data))) {
  m_int  <- MASS::polr(UB_ord ~ DG + MD + MIS_Int,  data = data, Hess = TRUE, method = "logistic")
  m_symb <- MASS::polr(UB_ord ~ DG + MD + MIS_Symb, data = data, Hess = TRUE, method = "logistic")
  print(summary(m_int));  print(exp(coef(m_int)));  print(exp(confint.default(m_int)))
  print(summary(m_symb)); print(exp(coef(m_symb))); print(exp(confint.default(m_symb)))
}

# Mediation (DG -> MD -> UB) --------------------------------------------------
# NOTE: mediation::mediate does not officially support polr; treat as exploratory.
try({
  UB_ordinal <- data$UB_ord
  med_model  <- lm(MD ~ DG, data = data)
  out_model  <- MASS::polr(UB_ordinal ~ DG + MD, data = data, method = "logistic", Hess = TRUE)

  med_out <- mediation::mediate(model.m = med_model, model.y = out_model,
                                treat = "DG", mediator = "MD", sims = 2000)
  print(summary(med_out))
  # Diagram (example labels)
  DiagrammeR::grViz("
  digraph mediation {
    graph [layout = dot, rankdir = LR]
    node [shape = rectangle, style = filled, fillcolor = white]
    DG [label = 'Dispositional Greed']
    MD [label = 'Moral Disengagement']
    UB [label = 'Unethical Behavior']
    DG -> MD [label = 'a']
    MD -> UB [label = 'b']
    DG -> UB [label = 'c\\'']
  }")
}, silent = TRUE)

# Moderation (MIS) ------------------------------------------------------------
mod_model <- MASS::polr(UB_ord ~ DG * MIS, data = data, method = "logistic", Hess = TRUE)
summary_mod <- summary(mod_model); print(summary_mod)
t_values <- coef(summary_mod)[, "t value"]
p_values <- 2 * pnorm(abs(t_values), lower.tail = FALSE)
print(data.frame(Estimate = coef(summary_mod)[, "Value"],
                 `Std. Error` = coef(summary_mod)[, "Std. Error"],
                 `t value` = t_values, `p value` = p_values))

DG_seq  <- seq(min(data$DG, na.rm=TRUE), max(data$DG, na.rm=TRUE), length.out = 100)
MIS_q   <- quantile(data$MIS, probs = c(.1,.5,.9), na.rm = TRUE)
pred    <- expand.grid(DG = DG_seq, MIS = as.numeric(MIS_q))
pred$Pr_first <- predict(mod_model, newdata = pred, type = "probs")[,1]
pred$MIS_Label <- factor(rep(c("Low","Median","High"), each = length(DG_seq)),
                         levels = c("Low","Median","High"))

p <- ggplot(pred, aes(DG, Pr_first, color = MIS_Label)) +
  geom_line() +
  labs(title = "Moderation of MIS on DG → UB (prob. of lowest UB category)",
       x = "Dispositional Greed", y = "Predicted probability", color = "MIS") +
  theme_minimal()
ggsave("outputs/figures/moderation_MIS.png", p, width = 7, height = 4, dpi = 300)

# Moderation (MIS_Int / MIS_Symb) if present ----------------------------------
if ("MIS_Int" %in% names(data)) {
  m_int <- MASS::polr(UB_ord ~ DG * MIS_Int, data = data, method="logistic", Hess=TRUE)
  print(summary(m_int))
}
if ("MIS_Symb" %in% names(data)) {
  m_symb <- MASS::polr(UB_ord ~ DG * MIS_Symb, data = data, method="logistic", Hess=TRUE)
  print(summary(m_symb))
}

# Moderated mediation (exploratory) -------------------------------------------
try({
  mm_med <- lm(MD ~ DG * MIS, data = data)
  mm_out <- MASS::polr(UB_ord ~ DG + MD + MIS + MD:MIS, data = data,
                       method = "logistic", Hess = TRUE)
  mod_med <- mediation::mediate(mm_med, mm_out, treat = "DG", mediator = "MD", sims = 2000)
  print(summary(mod_med))
}, silent = TRUE)

# ANCOVA (ranked), only if "group" exists -------------------------------------
if ("group" %in% names(data)) {
  data$rank_UB <- rank(data$UB, na.last = "keep")
  ancova_model <- aov(rank_UB ~ group + DG, data = data)
  print(summary(ancova_model))
}

# Diagnostics (linear model on numeric UB – heuristic) ------------------------
lm_check <- lm(UB ~ DG + MD + MIS, data = data)
plot(lm_check$fitted.values, resid(lm_check),
     main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals"); abline(h=0,col="red")
print(car::durbinWatsonTest(lm_check))
print(lmtest::bptest(lm_check))

# Session info ----------------------------------------------------------------
writeLines(capture.output(sessionInfo()), "outputs/tables/sessionInfo.txt")
message("Done. Plots in outputs/figures; session info in outputs/tables.")
