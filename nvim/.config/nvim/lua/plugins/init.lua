-- luacheck: globals vim
return {
	{ "echasnovski/mini.nvim", version = false },

	------------------------- UI -------------------------
	{ "catppuccin/nvim", name = "catppuccin" },
	"nvim-tree/nvim-web-devicons",
	"nvim-tree/nvim-tree.lua",
	"akinsho/bufferline.nvim",
	"goolord/alpha-nvim",
	"lewis6991/impatient.nvim",
	{ "folke/noice.nvim", dependencies = { "MunifTanjim/nui.nvim", "rcarriga/nvim-notify" } },

	--------------------------- UTILITIES ---------------------------

	------------------------- TELESCOPE -------------------------
	{ "nvim-telescope/telescope.nvim", dependencies = { "nvim-lua/plenary.nvim" } },
	"nvim-telescope/telescope-file-browser.nvim",
	{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
	"jvgrootveld/telescope-zoxide",

	{ "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
	"nvim-treesitter/nvim-treesitter-context",
	"nvim-treesitter/nvim-treesitter-textobjects",
	"nvim-treesitter/nvim-treesitter-refactor",

	"ThePrimeagen/harpoon",

	"mbbill/undotree",

	------------------------- LSP -------------------------
	{
		"VonHeikemen/lsp-zero.nvim",
		branch = "v1.x",
		dependencies = {
			-- LSP Support
			{ "neovim/nvim-lspconfig" },
			{ "williamboman/mason.nvim" },
			{ "williamboman/mason-lspconfig.nvim" },
			-- Autocompletion
			{ "hrsh7th/nvim-cmp" },
			{ "hrsh7th/cmp-buffer" },
			{ "hrsh7th/cmp-path" },
			-- { "saadparwaiz1/cmp_luasnip" },
			{ "hrsh7th/cmp-nvim-lsp" },
			{ "hrsh7th/cmp-nvim-lua" },
			-- Snippets
			-- { "L3MON4D3/LuaSnip" },
			-- { "rafamadriz/friendly-snippets" },
		},
	},
	"onsails/lspkind-nvim",
	{ "glepnir/lspsaga.nvim", event = "BufRead" },
	{ "folke/trouble.nvim", dependencies = "nvim-tree/nvim-web-devicons" },

	"ray-x/lsp_signature.nvim",

	"jose-elias-alvarez/null-ls.nvim",

	"lukas-reineke/indent-blankline.nvim",

	"ggandor/leap.nvim",

	"windwp/nvim-ts-autotag",

	"folke/twilight.nvim",

	"folke/zen-mode.nvim",

	{ "nvim-lualine/lualine.nvim" },

	"lewis6991/gitsigns.nvim",

	"sindrets/diffview.nvim",

	"akinsho/toggleterm.nvim",

	{
		"iamcco/markdown-preview.nvim",
		build = function()
			vim.fn["mkdp#util#install"]()
		end,
	},

	"github/copilot.vim",

	"folke/which-key.nvim",

	"norcalli/nvim-colorizer.lua",
}
