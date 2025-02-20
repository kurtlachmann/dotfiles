local M = {}

M.checkoutRecentBranch = function()
  -- Find branches sorted by recent checkouts
  local branches = {}
  local reflog = vim.fn.system({ "git", "reflog" })
  for branch in string.gmatch(reflog, "moving from (%g+)") do
    if not vim.tbl_contains(branches, branch) then
      table.insert(branches, branch)
    end
  end
  
  -- Open popup to select branch
  vim.ui.select(branches, {prompt = "Select branch"}, function(choice)
    if choice then
      vim.fn.system({ "git", "checkout", choice })
    end
  end)
end

return M
