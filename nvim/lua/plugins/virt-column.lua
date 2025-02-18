return {
  "lukas-reineke/virt-column.nvim",
  config = function()
    -- Create a new highlight group to customize the color
    vim.api.nvim_set_hl(0, "VirtColumn", { fg = "#252535" })

    require("virt-column").setup({
      char = "┃",
      virtcolumn = "101",
      highlight = "VirtColumn",
    })
  end,
}
