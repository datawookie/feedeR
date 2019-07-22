library(hexSticker)
library(showtext)

# Load Google font.
#
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()

sticker(here::here("inst/hex/rss.png"),
        # Image
        s_x = 1,
        s_y = 1.15,
        s_width = 0.5,
        s_height = 0.5,
        # Package name
        package = "{feedeR}",
        p_size = 16,
        p_y = 0.45,
        p_color = "#ffffff",
        p_family = "roboto_slab",
        # Heck
        h_fill = "#3498db",
        h_color = "#000000",
        # Spotlight
        # spotlight = TRUE,
        # l_y = 0.45,
        # l_alpha = 0.4,
        # Output
        filename = here::here("man/figures/feedeR-hex.png"),
        dpi = 300
)
