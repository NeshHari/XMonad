require("toggleterm").setup {
    open_mapping = [[<c-\>]],
    size = 25,
    shell = "fish",
    hide_numbers = true,
    close_on_exit = true,
}

local Terminal = require('toggleterm.terminal').Terminal
local lazygit  = Terminal:new({ cmd = "lazygit", hidden = true })

function _lazygit_toggle()
    lazygit:toggle()
end

vim.api.nvim_set_keymap("n", "<leader>lg", "<cmd>lua _lazygit_toggle()<CR>", { noremap = true, silent = true })
