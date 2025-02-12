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
          view = {
            { "n", "q", "<cmd>DiffviewClose<CR>", { desc = "Close Diffview" } },
          },
          file_panel = {
            { "n", "q", "<cmd>DiffviewClose<CR>", { desc = "Close Diffview" } },
          },
          file_history_panel = {
            { "n", "q", "<cmd>DiffviewClose<CR>", { desc = "Close Diffview" } },
          },
        },
      })
    end,
  },
}
