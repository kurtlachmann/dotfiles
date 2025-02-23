return {
  "nanozuki/tabby.nvim",
  dependencies = "nvim-tree/nvim-web-devicons",
  config = function()
    local theme = {
      fill = "TabLineFill",
      -- Also you can do this: fill = { fg='#f2e9de', bg='#907aa9', style='italic' }
      head = "TabLine",
      current_tab = "TabLineSel",
      tab = "TabLine",
      win = "TabLine",
      tail = "TabLine",
    }
    require("tabby").setup({
      line = function(line)
        return {
          line.tabs().foreach(function(tab)
            local hl = tab.is_current() and theme.current_tab or theme.tab
            return {
              " ",
              tab.name(),
              hl = hl,
              " ",
            }
          end),
          line.spacer(),
        }
      end,
    })
  end,
}
