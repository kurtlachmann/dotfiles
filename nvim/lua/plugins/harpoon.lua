return {
  "ThePrimeagen/harpoon",
  branch = "harpoon2",
  dependencies = { "nvim-lua/plenary.nvim" },
  config = function()
    local harpoon = require("harpoon")
    harpoon:setup()

    -- Add current buffer to harpoon list
    vim.keymap.set("n", "ha", function()
      harpoon:list():add()
    end)

    -- Show harpooned files
    vim.keymap.set("n", "hh", function()
      harpoon.ui:toggle_quick_menu(harpoon:list())
    end)

    -- Jump to harpooned file
    vim.keymap.set("n", "he", function()
      harpoon:list():select(1)
    end)
    vim.keymap.set("n", "hi", function()
      harpoon:list():select(2)
    end)
    vim.keymap.set("n", "ho", function()
      harpoon:list():select(3)
    end)
    vim.keymap.set("n", "h,", function()
      harpoon:list():select(4)
    end)
  end,
}
