library(coursekata)
library(ggformula)
library(ggplot2)
library(here)

here::i_am("reference/img_generators/gen_img_gf_lm.R")
img_dir <- here("reference", "img")

source(here("gf_lm_cat.R"))
source(here("gf_coef.R"))
source(here("gf_lm.R"))

save_plot <- function(p, filename, width = 4.5, height = 3) {
  ggsave(file.path(img_dir, filename), plot = p, width = width, height = height, dpi = 150)
  cat("saved:", filename, "\n")
}

# ── 1. Categorical x: two groups ─────────────────────────────────────────────
p1 <- gf_jitter(later_anxiety ~ condition, data = er, width = 0.1, alpha = 0.4) %>%
  gf_lm() +
  labs(title = "Categorical x: two groups")
save_plot(p1, "gf_lm_cat_2group.png")

# ── 2. Categorical x: three groups ───────────────────────────────────────────
Fingers3 <- droplevels(subset(Fingers, Year %in% c("1", "2", "3")))
p2 <- gf_jitter(Thumb ~ Year, data = Fingers3, width = 0.1, alpha = 0.4) %>%
  gf_lm() +
  labs(title = "Categorical x: three groups", x = "Year in college", y = "Thumb (mm)")
save_plot(p2, "gf_lm_3group.png")

# ── 3. Continuous x ───────────────────────────────────────────────────────────
p3 <- gf_point(Thumb ~ Height, data = Fingers, alpha = 0.4) %>%
  gf_lm() +
  labs(title = "Continuous x: regression line")
save_plot(p3, "gf_lm_cont.png")

# ── 4. Chained with gf_coef ──────────────────────────────────────────────────
p4 <- gf_jitter(later_anxiety ~ condition, data = er, width = 0.1, alpha = 0.4) %>%
  gf_lm() %>%
  gf_coef() +
  labs(title = "Chained with gf_coef()")
save_plot(p4, "gf_lm_with_coef.png")

# ── 5. Shuffled data ──────────────────────────────────────────────────────────
set.seed(7)
p5 <- gf_jitter(shuffle(later_anxiety) ~ condition, data = er,
                width = 0.1, alpha = 0.4) %>%
  gf_lm() +
  labs(title = "Shuffled y: segments reflect the shuffle")
save_plot(p5, "gf_lm_shuffle.png")

# ── 6. Polynomial fit ─────────────────────────────────────────────────────────
p6 <- gf_point(Thumb ~ Height, data = Fingers, alpha = 0.4) %>%
  gf_lm(formula = y ~ poly(x, 2)) +
  labs(title = "Polynomial fit: formula = y ~ poly(x, 2)")
save_plot(p6, "gf_lm_poly.png")

cat("\nAll images saved to", img_dir, "\n")
