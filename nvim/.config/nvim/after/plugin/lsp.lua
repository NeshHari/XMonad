local lsp = require('lsp-zero')
lsp.preset('recommended')

lsp.setup()

require("mason").setup({
})
require("mason-lspconfig").setup({
})

--local util = require("lspconfig/util")

require 'lspconfig'.hls.setup {
}
require 'lspconfig'.sumneko_lua.setup {
    settings = {
        Lua = {
            diagnostics = {
                globals = { 'vim' }
            }
        }
    }
}

local opts = { noremap = true, silent = true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)
