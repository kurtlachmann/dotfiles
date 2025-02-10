-- Close with Leader-q
vim.keymap.set('n', '<leader>q', ':q<CR>', { desc = 'Close file' })

-- Save file with Ctrl-s
vim.keymap.set('n', '<C-s>', ':w<CR>')

-- Copy/paste to or from system clipboard with the usual shortcuts
-- TODO call `:set paste` before pasting and `:set nopaste` afterwards to keep formatting intact
vim.keymap.set('v', '<C-c>', '"+y')
vim.keymap.set('i', '<C-v>', '<C-r>+')

-- Continue last f,F,t,T in the same direction with , and
-- in the opposite direction with ;
-- Essentially swapping the default behaviour.
vim.keymap.set('n', ',', ';')
vim.keymap.set('n', ';', ',')

-- Move up/down in bigger chunks
vim.keymap.set({'n', 'v'}, '<C-up>', '8<up>')
vim.keymap.set({'n', 'v'}, '<C-down>', '8<down>')

-- Move left/right by words with Ctrl-Left/Right
vim.keymap.set('n', '<C-right>', 'w')
vim.keymap.set('n', '<C-left>', 'b')

-- Move to first non-whitespace character with <Home>
vim.keymap.set('i', '<Home>', '<C-O>^')
vim.keymap.set('n', '<Home>', '^')

