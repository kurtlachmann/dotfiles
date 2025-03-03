return {
  "OXY2DEV/markview.nvim",
  lazy = false,
  config = function()
    local presets = require("markview.presets")
    require("markview").setup({
      markdown = {
        headings = {
          enable = true,
          shift_width = 0,

          heading_1 = {
            style = "label",
            sign = "󰌕 ",
            sign_hl = "MarkviewHeading1Sign",

            padding_left = "╾─────────────╴ ",
            padding_right = " ╶─────────────╼",
            icon = "",
            hl = "MarkviewHeading1Sign",
          },
          heading_2 = {
            style = "label",
            sign = "󰌕 ",
            sign_hl = "MarkviewHeading2Sign",

            padding_left = "╾─────╴ ",
            padding_right = " ╶─────╼",
            icon = "",
            hl = "MarkviewHeading2Sign",
          },
          heading_3 = {
            style = "label",

            padding_left = "╾────╴ ",
            padding_right = " ╶────╼",
            icon = "",
            hl = "MarkviewHeading3Sign",
          },
          heading_4 = {
            style = "label",

            padding_left = "╾───╴ ",
            padding_right = " ╶───╼",
            icon = "",
            hl = "MarkviewHeading4Sign",
          },
          heading_5 = {
            style = "label",

            padding_left = "╾──╴ ",
            padding_right = " ╶──╼",
            icon = "",
            hl = "MarkviewHeading5Sign",
          },
          heading_6 = {
            style = "label",

            padding_left = "╾─╴ ",
            padding_right = " ╶─╼",
            icon = "",
            hl = "MarkviewHeading6Sign",
          },
        },
      },
    })
  end,
}
