local mark = require("harpoon.mark")
local ui = require("harpoon.ui")
local telescope = require("telescope")

vim.keymap.set("n", "<leader>ha", mark.add_file)
vim.keymap.set("n", "<leader>he", ui.toggle_quick_menu)
vim.keymap.set("n", "<leader>hj", ui.nav_next)
vim.keymap.set("n", "<leader>hk", ui.nav_prev)
vim.keymap.set("n", "<leader>hm", telescope.extensions.harpoon.marks)
