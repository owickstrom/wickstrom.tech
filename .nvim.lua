local root = vim.fn.fnamemodify(
  debug.getinfo(1, "S").source:gsub("^@", ""),
  ":p:h"
)
vim.notify("Root: " .. root, vim.log.levels.INFO) -- Add this to debug

vim.opt.spellfile = { root .. "/test/custom.utf-8.add" }
vim.opt.spelllang = "en"
