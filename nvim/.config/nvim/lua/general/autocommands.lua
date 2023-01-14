vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.format({async = false})]]

vim.cmd [[autocmd BufRead,BufNewFile *.md setlocal spell]]
