-- luacheck: globals vim
vim.g.copilot_assume_mapped = true

vim.api.nvim_set_keymap("i", "<C-J>", 'copilot#Accept("<CR>")', { silent = true, expr = true })

vim.g.copilot_filetypes = { markdown = false }
