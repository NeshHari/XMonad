local key = vim.keymap

vim.g.mapleader = " "

key.set("n", "<leader>pv", vim.cmd.Ex)
key.set("v", "J", ":m '>+1<CR>gv=gv")
key.set("v", "K", ":m '<-2<CR>gv=gv")
key.set("x", "<leader>p", [["_dP]])
key.set({ "n", "v" }, "<leader>y", [["+y]])
key.set("n", "<leader>Y", [["+Y]])
key.set({ "n", "v" }, "<leader>d", [["_d]])
key.set("n", "<leader>fo", vim.lsp.buf.format)
