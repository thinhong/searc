compute_pct_by_country <- function(data, country, col) {
  data |>
    group_by({{country}}, {{col}}) |>
    count() |>
    group_by({{country}}) |>
    mutate(pct = 100 * n / sum(n))
}

plot_pct_by_country <- function(data, country, col, pct, n) {
  ggplot(data, aes(x = fct_rev({{col}}), y = {{pct}})) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0({{n}}, " (", round({{pct}}, 0), "%)")), hjust = -0.1, size = 3) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    coord_flip() +
    facet_wrap(vars({{country}})) +
    labs(x = NULL, y = "Percentage") +
    theme_bw()
}

bar_group_country <- function(data, group, country, feat) {
  data |> 
    group_by({{country}}, {{group}}) |> 
    count({{feat}}) |> 
    group_by({{country}}, {{group}}) |>
    mutate(pct = 100 * n / sum(n)) |> 
    ggplot(aes(x = fct_rev({{feat}}), y = pct)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(n, " (", round(pct, 0), "%)")), hjust = -0.1, size = 3) +
    coord_flip(ylim = ) +
    facet_grid(cols = vars({{group}}), rows = vars({{country}})) +
    scale_y_continuous(expand = expansion(add = c(0, 30))) +
    labs(x = NULL, y = "Percentage") +
    theme_bw()
}
