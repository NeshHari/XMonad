-- luacheck: globals vim

require("catppuccin").setup({
	flavour = "mocha",
	term_colors = true,
	integrations = {
		cmp = true,
		nvimtree = true,
		telescope = true,
		notify = true,
		leap = true,
		mason = true,
		harpoon = true,
		treesitter = true,
		treesitter_context = true,
		indent_blankline = {
			enabled = true,
			colored_indent_levels = true,
		},
		native_lsp = {
			enabled = true,
			virtual_text = {
				errors = { "italic" },
				hints = { "italic" },
				warnings = { "italic" },
				information = { "italic" },
			},
			underlines = {
				errors = { "underline" },
				hints = { "underline" },
				warnings = { "underline" },
				information = { "underline" },
			},
		},
		lsp_saga = true,
		which_key = true,
		markdown = true,
	},
	color_overrides = {
		mocha = {
			base = "#050508",
		},
	},
	transparent_background = true,
	dim_inactive = {
		enabled = false,
		shade = "dark",
		percentage = 0.15,
	},
})

vim.cmd.colorscheme("catppuccin")
