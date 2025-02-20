return {
  {
    "sindrets/diffview.nvim",
    config = function()
      -- Replace the characters that are used to fill missing lines
      vim.opt.fillchars:append({ diff = "╱" })

      -- git log
      vim.keymap.set("n", "<leader>gl", "<cmd>DiffviewFileHistory<CR>", { desc = "Git log" })
      vim.keymap.set(
        "n",
        "<leader>gh",
        "<cmd>DiffviewFileHistory %<CR>",
        { desc = "History of current file" }
      )

      local actions = require("diffview.config").actions
      local commonKeymaps = {
        { "n", "q", "<cmd>DiffviewClose<CR>", { desc = "Close Diffview" } },
        {
          "n",
          "<c-f>",
          actions.scroll_view(-10),
          { desc = "Scroll up" },
        },
        {
          "n",
          "<c-s>",
          actions.scroll_view(10),
          { desc = "Scroll down" },
        },
      }

      -- git status
      vim.keymap.set("n", "<leader>gs", "<cmd>DiffviewOpen -uno<CR>", { desc = "Git status" })
      vim.keymap.set(
        "n",
        "<leader>gS",
        "<cmd>DiffviewOpen<CR>",
        { desc = "Git status with untracked files" }
      )

      require("diffview").setup({
        enhanced_diff_hl = true,
        keymaps = {
          view = commonKeymaps,
          file_panel = commonKeymaps,
          file_history_panel = commonKeymaps,
        },
        hooks = {
          diff_buf_read = function(bufnr)
            -- Cursorline has underline when current line is on a diff -> Disable it
            -- https://github.com/sindrets/diffview.nvim/issues/113
            vim.opt_local.cursorline = false
          end,
        },
      })
    end,
  },
}
