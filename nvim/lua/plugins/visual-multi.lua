return {
  "mg979/vim-visual-multi",
  init = function()
    vim.g.VM_default_mappings = 0
    vim.g.VM_maps = {
      ["Add Cursor Up"] = "<M-Up>",
      ["Add Cursor Down"] = "<M-Down>",
      ["Find Under"] = "<C-D>",
      ["Find Subword Under"] = "<C-D>",
    }
  end,
}
