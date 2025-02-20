return {
  "samjwill/nvim-unception",
  init = function()
    -- This let's us run `git commit` from inside a neovim terminal without spawning a nested
    -- neovim instance. Instead the commit message can be composed in a normal buffer in the
    -- current editor.
    -- This assumes that you're not overwriting GIT_EDITOR in your .bashrc or .zshrc
    vim.env.GIT_EDITOR = "nvim --cmd 'let g:unception_block_while_host_edits=1'"
  end,
}
