local root = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h")
vim.opt.spellfile = root .. "/../bombadil-playground/wickstrom.tech/custom.utf-8.add"
vim.opt.spelllang = "en"
