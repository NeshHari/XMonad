-- luacheck: globals vim
local opt = vim.opt

opt.nu = true
opt.relativenumber = true

opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.smartindent = true

opt.ignorecase = true
opt.smartcase = true

opt.hlsearch = false
opt.incsearch = true

opt.termguicolors = true

opt.scrolloff = 8
opt.signcolumn = "yes"

opt.updatetime = 50

opt.mouse = "a"
opt.swapfile = false

opt.completeopt = "menu,menuone,noselect,noinsert"

opt.clipboard = "unnamedplus"

opt.list = true

opt.timeoutlen = 1000
opt.ttimeoutlen = 1000

opt.listchars:append("space:⋅")
opt.listchars:append("eol:↴")

opt.spelllang = "en_uk"
