return {
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    opts = {},
    init = function()
      vim.cmd.colorscheme("tokyonight-night")

      vim.api.nvim_set_hl(0, "DiffAdd", { bg = "#234c3d" })
      vim.api.nvim_set_hl(0, "DiffDelete", { bg = "#592c42" })
      vim.api.nvim_set_hl(0, "DiffChange", { bg = "#1f2231" })
      vim.api.nvim_set_hl(0, "DiffText", { bg = "#394b70" })
    end,
  },
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
  },
}
