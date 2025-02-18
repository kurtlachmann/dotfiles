-- Close with Leader-q
vim.keymap.set("n", "<leader>q", ":q<CR>", { desc = "Close file" })

-- Save file with Ctrl-s
vim.keymap.set("n", "<C-s>", ":w<CR>")
vim.keymap.set("i", "<C-s>", "<ESC>:w<CR>")

-- Copy/paste to or from system clipboard with the usual shortcuts
-- TODO call `:set paste` before pasting and `:set nopaste` afterwards to keep formatting intact
vim.keymap.set("v", "<C-c>", '"+y')
vim.keymap.set("i", "<C-v>", "<C-r>+")

-- Continue last f,F,t,T in the same direction with , and
-- in the opposite direction with ;
-- Essentially swapping the default behaviour.
vim.keymap.set("n", ",", ";")
vim.keymap.set("n", ";", ",")

-- Move up/down in bigger chunks
vim.keymap.set({ "n", "v" }, "<C-up>", "5<up>")
vim.keymap.set({ "n", "v" }, "<C-down>", "5<down>")

-- Move left/right in chunks with Ctrl-Left/Right
vim.keymap.set("n", "<C-right>", "10<right>")
vim.keymap.set("n", "<C-left>", "10<left>")

-- Move to first non-whitespace character with <Home>
vim.keymap.set("i", "<Home>", "<C-O>^")
vim.keymap.set("n", "<Home>", "^")

-- Switch tabs with Ctrl+PageUp/Down
vim.keymap.set("n", "<C-PgUp", ":tabNext")
vim.keymap.set("n", "<C-PgDown", ":tabprevious")

-- Exit terminal mode with Escape
vim.keymap.set("t", "<Esc>", "<C-\\><C-n>")

-- Clear highlights on search when pressing <Esc> in normal mode
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- Toggle line comments with <Leader><Comma>
vim.keymap.set("n", "<leader>,", "gcc", { remap = true, desc = "Toggle comment" })
vim.keymap.set("v", "<leader>,", "gc", { remap = true, desc = "Toggle comment" })

-- Enter visual line mode with <Leader>V
vim.keymap.set("n", "<leader>v", "V", { desc = "Visual line mode" })

-- Wildmenu command completion: use <Up>/<Down> to navigate completion items
vim.keymap.set("c", "<down>", function()
  return vim.fn.wildmenumode() == 1 and "<right>" or "<down>"
end, { expr = true })
vim.keymap.set("c", "<up>", function()
  return vim.fn.wildmenumode() == 1 and "<left>" or "<up>"
end, { expr = true })

-- Jump back/forward with Alt-Left/Right
vim.keymap.set("n", "<M-left>", "<C-o>")
vim.keymap.set("n", "<M-right>", "<C-i>")
