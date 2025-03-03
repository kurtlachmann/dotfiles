-- Modify a highlight group.
-- Neovim only offers vim.api.nvim_set_hl() which completely replaces the highlights.
local function mod_hl(hl_name, opts)
  local is_ok, hl_def = pcall(vim.api.nvim_get_hl_by_name, hl_name, true)
  if is_ok then
    for k, v in pairs(opts) do
      hl_def[k] = v
    end
    vim.api.nvim_set_hl(0, hl_name, hl_def)
  end
end

return {
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("lualine").setup({
        options = {
          section_separators = { left = "", right = "" },
          component_separators = { left = "", right = "" },
          theme = "ayu",
          globalstatus = true,
        },
        sections = {
          lualine_a = { "mode" },
          lualine_b = { { "filename", path = 1 } },
          lualine_c = { },
          lualine_x = { "branch", "encoding", "fileformat", "filetype" },
          lualine_y = { "progress" },
          lualine_z = { "location" },
        },
      })
      -- Make sure the mode is bold
      -- mod_hl("lualine_a_normal", { bold = true })
      -- mod_hl("lualine_a_insert", { bold = true })
      -- mod_hl("lualine_a_visual", { bold = true })
      -- mod_hl("lualine_a_terminal", { bold = true })
      -- mod_hl("lualine_a_replace", { bold = true })
      -- mod_hl("lualine_a_command", { bold = true })
    end,
  },
}
