local reload_harpoon = function()
  local hok, Harpoon = pcall(require, "harpoon")
  local dok, Data = pcall(require, "harpoon.data")
  if not hok or not dok then
    return
  end
  Harpoon.data = Data.Data:new(Harpoon.config)
  Harpoon.lists = {}
end

return {
  "ThePrimeagen/harpoon",
  branch = "harpoon2",
  dependencies = { "nvim-lua/plenary.nvim" },
  init = function()
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

    -- Reload harpoon when the current working directory has changed
    vim.api.nvim_create_autocmd({ "DirChanged" }, {
      pattern = "*",
      callback = reload_harpoon,
    })
  end,
}
