-- luacheck: globals vim
local key = vim.keymap.set
local opts = { noremap = true, silent = true }

key("n", "<leader>dvo", ":DiffviewOpen<CR>", opts)
key("n", "<leader>dvc", ":DiffviewClose<CR>", opts)
key("n", "<leader>dvt", ":DiffviewToggleFiles<CR>", opts)
key("n", "<leader>dvl", ":DiffviewLog<CR>", opts)
key("n", "<leader>dvf", ":DiffviewFocusFiles<CR>", opts)
key("n", "<leader>dvh", ":DiffviewFileHistory<CR>", opts)
key("n", "<leader>dvr", ":DiffviewRefresh<CR>", opts)
