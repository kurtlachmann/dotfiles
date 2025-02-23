return { -- Fuzzy Finder (files, lsp, etc)
  "nvim-telescope/telescope.nvim",
  event = "VimEnter",
  dependencies = {
    "nvim-lua/plenary.nvim",
    { -- If encountering errors, see telescope-fzf-native README for installation instructions
      "nvim-telescope/telescope-fzf-native.nvim",

      -- `build` is used to run some command when the plugin is installed/updated.
      -- This is only run then, not every time Neovim starts up.
      build = "make",
    },
    { "natecraddock/telescope-zf-native.nvim" },
    { "nvim-telescope/telescope-ui-select.nvim" },

    -- Useful for getting pretty icons, but requires a Nerd Font.
    { "nvim-tree/nvim-web-devicons", enabled = vim.g.has_nerd_font },
  },
  config = function()
    require("telescope").setup({

      defaults = {
        path_display = { "filename_first" },
      },

      extensions = {
        ["ui-select"] = {
          require("telescope.themes").get_dropdown(),
        },
      },
    })

    -- Enable Telescope extensions if they are installed
    pcall(require("telescope").load_extension, "zf-native")
    pcall(require("telescope").load_extension, "ui-select")

    local builtin = require("telescope.builtin")
    local map = vim.keymap.set
    map("n", "<leader>ff", builtin.find_files, { desc = "Find files" })
    map("n", "<leader>fF", builtin.live_grep, { desc = "Find in files" })
    map("n", "<leader>fb", function()
      builtin.buffers({ sort_mru = true, sort_lastused = true })
    end, { desc = "Find buffers" })
  end,
}
