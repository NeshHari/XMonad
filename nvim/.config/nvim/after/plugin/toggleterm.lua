-- luacheck: globals _lazygit_toggle vim
require("toggleterm").setup({
	open_mapping = [[<c-\>]],
	shell = "fish",
	hide_numbers = true,
	close_on_exit = true,
	persit_size = false,
	auto_scroll = true,
	start_in_insert = true,
	direction = "float",
	float_opts = {
		border = "curved",
		width = 80,
		height = 40,
	},
})

local Terminal = require("toggleterm.terminal").Terminal
local lazygit = Terminal:new({ cmd = "lazygit", hidden = true })

function _lazygit_toggle()
	lazygit:toggle()
end

vim.api.nvim_set_keymap("n", "<leader>lg", "<cmd>lua _lazygit_toggle()<CR>", { noremap = true, silent = true })
