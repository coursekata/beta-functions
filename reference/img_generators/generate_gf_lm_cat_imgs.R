library(coursekata)
library(ggformula)
library(ggplot2)
library(here)

here::i_am("reference/img_generators/generate_gf_lm_cat_imgs.R")
img_dir <- here("reference", "img")

source(here("gf_lm_cat.R"))
source(here("gf_coef.R"))

save_plot <- function(p, filename, width = 4.5, height = 3) {
  ggsave(file.path(img_dir, filename), plot = p, width = width, height = height, dpi = 150)
  cat("saved:", filename, "\n")
}

# ── 1. Two groups, default style ─────────────────────────────────────────────
p1 <- gf_jitter(later_anxiety ~ condition, data = er, width = 0.1, alpha = 0.4) %>%
  gf_lm_cat() +
  labs(title = "Two groups: default style")
save_plot(p1, "gf_lm_cat_2group.png")

# ── 2. Three groups ───────────────────────────────────────────────────────────
Fingers3 <- droplevels(subset(Fingers, Year %in% c("1", "2", "3")))
p2 <- gf_jitter(Thumb ~ Year, data = Fingers3, width = 0.1, alpha = 0.4) %>%
  gf_lm_cat() +
  labs(title = "Three groups", x = "Year in college", y = "Thumb (mm)")
save_plot(p2, "gf_lm_cat_3group.png")

# ── 3. Chained with gf_coef ──────────────────────────────────────────────────
p3 <- gf_jitter(later_anxiety ~ condition, data = er, width = 0.1, alpha = 0.4) %>%
  gf_lm_cat() %>%
  gf_coef() +
  labs(title = "Chained with gf_coef()")
save_plot(p3, "gf_lm_cat_with_coef.png")

# ── 4. Shuffled data ──────────────────────────────────────────────────────────
set.seed(7)
p4 <- gf_jitter(shuffle(later_anxiety) ~ condition, data = er,
                width = 0.1, alpha = 0.4) %>%
  gf_lm_cat() +
  labs(title = "Shuffled y: segments reflect the shuffle")
save_plot(p4, "gf_lm_cat_shuffle.png")

gf_jitter(shuffle(later_anxiety) ~ condition, data = er,
          width = 0.1, alpha = 0.4) %>%
  gf_lm_cat()

# ── 5. Custom width ───────────────────────────────────────────────────────────
p5 <- gf_jitter(later_anxiety ~ condition, data = er, width = 0.1, alpha = 0.4) %>%
  gf_lm_cat(width = 0.8) +
  labs(title = "Wider segments: width = 0.8")
save_plot(p5, "gf_lm_cat_width.png")

# ── 6. Custom color ───────────────────────────────────────────────────────────
p6 <- gf_jitter(later_anxiety ~ condition, data = er, width = 0.1, alpha = 0.4) %>%
  gf_lm_cat(color = "blue") +
  labs(title = "Custom color: blue")
save_plot(p6, "gf_lm_cat_color.png")

cat("\nAll images saved to", img_dir, "\n")
