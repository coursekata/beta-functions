library(coursekata)
library(ggformula)
library(ggplot2)

source("https://raw.githubusercontent.com/coursekata/beta-functions/refs/heads/main/gf_lm_cat.R")
source("https://raw.githubusercontent.com/coursekata/beta-functions/refs/heads/main/gf_coef.R")

library(here)
img_dir <- here("reference", "img")

save_plot <- function(p, filename, width = 4.5, height = 3) {
  ggsave(file.path(img_dir, filename), plot = p, width = width, height = height, dpi = 150)
  cat("saved:", filename, "\n")
}

# ── 1. Categorical x: two groups ─────────────────────────────────────────────
p1 <- gf_jitter(later_anxiety ~ condition, data = er, width = 0.1, alpha = 0.4) %>%
  gf_lm_cat() %>%
  gf_coef() +
  labs(title = "Categorical x: two groups")
save_plot(p1, "gf_coef_cat_2group.png")

# ── 2. Categorical x: three groups ───────────────────────────────────────────
# Year in college (1, 2, 3) from Fingers
Fingers3 <- droplevels(subset(Fingers, Year %in% c("1", "2", "3")))
p2 <- gf_jitter(Thumb ~ Year, data = Fingers3, width = 0.1, alpha = 0.4) %>%
  gf_lm_cat() %>%
  gf_coef() +
  labs(title = "Categorical x: three groups", x = "Year in college", y = "Thumb (mm)")
save_plot(p2, "gf_coef_cat_3group.png")

# ── 3. Continuous x: auto run ─────────────────────────────────────────────────
p3 <- gf_point(Thumb ~ Height, data = Fingers, alpha = 0.4) %>%
  gf_lm() %>%
  gf_coef() +
  labs(title = "Continuous x: auto run")
save_plot(p3, "gf_coef_cont_auto.png")

# ── 4. Continuous x: forced run = 10 ─────────────────────────────────────────
p4 <- gf_point(Thumb ~ Height, data = Fingers, alpha = 0.4) %>%
  gf_lm() %>%
  gf_coef(run = 10) +
  labs(title = "Continuous x: run = 10")
save_plot(p4, "gf_coef_cont_run10.png")

# ── 5. b0 far outside data range (synthetic — no coursekata equivalent) ───────
set.seed(42)
n <- 80
d_far <- data.frame(x = rnorm(n, mean = 67, sd = 4))
d_far$y <- -3 + 0.9 * d_far$x + rnorm(n, sd = 3)
p5 <- gf_point(y ~ x, data = d_far, alpha = 0.4) %>%
  gf_lm() %>%
  gf_coef() +
  labs(title = "b0 far outside data range", x = "Height (in)", y = "Thumb (mm)")
save_plot(p5, "gf_coef_cont_b0far.png")

# ── 6. Shuffled data ──────────────────────────────────────────────────────────
set.seed(7)
p6 <- gf_jitter(shuffle(later_anxiety) ~ condition, data = er,
                width = 0.1, alpha = 0.4) %>%
  gf_lm_cat() %>%
  gf_coef() +
  labs(title = "Shuffled data: arrows match the dots")
save_plot(p6, "gf_coef_shuffle.png")

# ── 7. show_b0_label = FALSE ──────────────────────────────────────────────────
p7 <- gf_jitter(later_anxiety ~ condition, data = er, width = 0.1, alpha = 0.4) %>%
  gf_lm_cat() %>%
  gf_coef(show_b0_label = FALSE) +
  labs(title = "show_b0_label = FALSE: b1 only")
save_plot(p7, "gf_coef_no_b0.png")

# ── 8. Custom color ───────────────────────────────────────────────────────────
p8 <- gf_jitter(later_anxiety ~ condition, data = er, width = 0.1, alpha = 0.4) %>%
  gf_lm_cat(color = "blue") %>%
  gf_coef(color = "blue") +
  labs(title = "Custom color: blue")
save_plot(p8, "gf_coef_custom_color.png")

cat("\nAll images saved to", img_dir, "\n")
