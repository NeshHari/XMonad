-- luacheck: globals vim
local builtin = require("telescope.builtin")
local z_utils = require("telescope._extensions.zoxide.utils")
local trouble = require("trouble.providers.telescope")
local telescope = require("telescope")

telescope.setup({
	defaults = {
		previewer = true,
	},
	extensions = {
		fzf = {
			fuzzy = true,
			override_generic_sorter = true,
			override_file_sorter = true,
			case_mode = "smart_case",
		},
		zoxide = {
			prompt_title = "[ Whatcha lookin' 4? ]",
			list_command = "zoxide query -ls",
			mappings = {
				default = {
					action = function(selection)
						vim.cmd("cd " .. selection.path)
					end,
					after_action = function(selection)
						print("Directory changed to " .. selection.path)
					end,
				},
				["<C-s>"] = { action = z_utils.create_basic_command("split") },
				["<C-v>"] = { action = z_utils.create_basic_command("vsplit") },
				["<C-e>"] = { action = z_utils.create_basic_command("edit") },
				["<C-b>"] = {
					keepinsert = true,
					action = function(selection)
						telescope.extensions.file_browser.file_browser({ cwd = selection.path })
					end,
				},
				["<C-f>"] = {
					keepinsert = true,
					action = function(selection)
						builtin.find_files({ cwd = selection.path })
					end,
				},
				i = { ["<c-t>"] = trouble.open_with_trouble },
				n = { ["<c-t>"] = trouble.open_with_trouble },
			},
		},
		file_browser = {
			-- disables netrw and use telescope-file-browser in its place
			hijack_netrw = true,
			mappings = {
				["i"] = {
					-- your custom insert mode mappings
				},
				["n"] = {
					-- your custom normal mode mappings
				},
			},
		},
	},
})

telescope.load_extension("fzf")
telescope.load_extension("zoxide")
telescope.load_extension("file_browser")

local keymap = vim.keymap.set

keymap("n", "<leader>ff", builtin.find_files, {})
keymap("n", "<leader>fw", builtin.live_grep, {})
keymap("n", "<leader>fb", telescope.extensions.file_browser.file_browser)
keymap("n", "<leader>fh", builtin.help_tags, {})
keymap("n", "<leader>bf", builtin.buffers, {})
keymap("n", "<leader>di", "<cmd>Telescope diagnostics<CR>", {})
keymap("n", "<leader>cd", telescope.extensions.zoxide.list)
keymap("n", "<leader>h", telescope.extensions.notify.notify)
