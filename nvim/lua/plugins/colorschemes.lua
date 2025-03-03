return {
  {
    "Shatur/neovim-ayu",
    lazy = false,
    priority = 1000,
    init = function()
      local ayu = require("ayu.colors")
      ayu.generate()

      local custom = {
        normal = "#dfddd6",
      }

      require("ayu").setup({
        overrides = function()
          return {
            Normal = { fg = custom.normal },
            ["@variable"] = { fg = custom.normal },
            ["@variable.parameter"] = { fg = custom.normal },
            LineNr = { fg = "#424853" },
            DiffAdd = { bg = "#25462e" },
            DiffDelete = { bg = "#4a2121" },
            DiffText = { bg = "#23447b" },
            MarkviewCheckboxChecked = { fg = ayu.string },
            MarkviewCheckboxUnchecked = { fg = custom.normal },

            -- Make the "/////" rows for diffview more subtle
            DiffviewDiffDeleteDim = { fg = "#1e222a" },
          }
        end,
      })

      -- Make this the default color scheme
      vim.cmd.colorscheme("ayu")
    end,
  },
  {
    "folke/tokyonight.nvim",
    priority = 1000,
  },
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
  },
  {
    "AlexvZyl/nordic.nvim",
    priority = 1000,
  },
}
