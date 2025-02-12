-- Force English UI in case the operating system uses a different language
vim.cmd("language en_IE.UTF8")

-- Enable line numbers
vim.opt.number = true

-- Use case insensitive search, except when using capital letters
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Keep signcolumn on by default
vim.opt.signcolumn = "yes"

-- Don't show the mode, since it's already in the status line
vim.opt.showmode = false

-- New windows should be opened below or right
vim.opt.splitright = true
vim.opt.splitbelow = true

-- Sets how neovim will display certain whitespace characters in the editor.
--  See `:help 'list'`
--  and `:help 'listchars'`
vim.opt.list = true
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }

-- Preview substitutions live, as you type!
vim.opt.inccommand = "split"

-- Show which line your cursor is on
vim.opt.cursorline = true

-- Minimal number of screen lines to keep above and below the cursor.
vim.opt.scrolloff = 10

-- Disable the tilde (~) lines that indicate the end of the buffer
vim.opt.fillchars = "eob: "

-- Indentation settings for using hard tabs for indent. Display tabs as four characters wide.
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true

-- Set terminal
if vim.fn.has("win32") == 1 then
  vim.o.shell = vim.fn.expand("$LOCALAPPDATA\\Programs\\Git\\bin\\bash.exe")
end

-------------------------------------------------
--------------- neovide settings ----------------
-------------------------------------------------
if vim.g.neovide == true then
  -- Disable animations
  vim.g.neovide_position_animation_length = 0
  vim.g.neovide_cursor_animation_length = 0.00
  vim.g.neovide_cursor_trail_size = 0
  vim.g.neovide_cursor_animate_in_insert_mode = false
  vim.g.neovide_cursor_animate_command_line = false
  vim.g.neovide_scroll_animation_far_lines = 0
  vim.g.neovide_scroll_animation_length = 0.00

  -- Fullscreen with F11
  vim.api.nvim_set_keymap("n", "<F11>", ":let g:neovide_fullscreen = !g:neovide_fullscreen<CR>", {})

  -- Set font
  vim.o.guifont = "Source Code Pro:h11"
end
