-- Neo-tree is a Neovim plugin to browse the file system
-- https://github.com/nvim-neo-tree/neo-tree.nvim

return {
  'nvim-neo-tree/neo-tree.nvim',
  version = '*',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'nvim-tree/nvim-web-devicons', -- not strictly required, but recommended
    'MunifTanjim/nui.nvim',
  },
  cmd = 'Neotree',
  keys = {
    { '<Leader>b', ':Neotree reveal<CR>', desc = 'NeoTree reveal', silent = true },
    { '<Leader>B', ':Neotree toggle<CR>', desc = 'NeoTree toggle', silent = true },
  },
  opts = {
    popup_border_style = 'rounded',
    filesystem = {
      window = {
        position = 'float',
        mappings = {
          ["<Left>"] = function(state)
            local node = state.tree:get_node()
            require("neo-tree.ui.renderer").focus_node(state, node:get_parent_id())
          end,
          ["<Right>"] = 'open',
        },
        popup = {
          title = '',
	  size = {
            width = 80,
	    height = 30,
	  },
        },
      },
    },
  },
}
