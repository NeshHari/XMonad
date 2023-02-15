-- luacheck: globals vim
local key = vim.keymap

vim.g.mapleader = " "

-- move whilst highlighted
key.set("v", "J", ":m '>+1<CR>gv=gv")
key.set("v", "K", ":m '<-2<CR>gv=gv")

-- paste over
key.set("x", "<leader>p", [["_dP]])

-- format
key.set("n", "<leader>fo", vim.lsp.buf.format)

-- navigate buffers
key.set("n", "<leader>gt", "<cmd>bnext<CR>")
key.set("n", "<leader>gT", "<cmd>bprev<CR>")

-- splits
key.set("n", "<leader>hs", "<cmd>split<CR>")
key.set("n", "<leader>vs", "<cmd>vsplit<CR>")
key.set("n", "<leader>cs", "<cmd>close<CR>")

-- wrapped lines
key.set({ "n", "v" }, "j", "gj")
key.set({ "n", "v" }, "k", "gk")
