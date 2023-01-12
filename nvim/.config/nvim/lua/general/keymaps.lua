local key = vim.keymap

vim.g.mapleader = " "

key.set("n", "<leader>pv", vim.cmd.Ex)
-- move whilst highlighted
key.set("v", "J", ":m '>+1<CR>gv=gv")
key.set("v", "K", ":m '<-2<CR>gv=gv")

-- paste over
key.set("x", "<leader>p", [["_dP]])

-- format
key.set("n", "<leader>fo", vim.lsp.buf.format)
