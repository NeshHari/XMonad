return {
    { 'catppuccin/nvim',
        as = 'catppuccin',
        dependencies = {
            'akinsho/bufferline.nvim',
            dependencies = { 'nvim-tree/nvim-web-devicons' },
        }
    },
    'goolord/alpha-nvim',
    { 'nvim-telescope/telescope.nvim', dependencies = { 'nvim-lua/plenary.nvim' } },
    'nvim-telescope/telescope-file-browser.nvim',
    { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
    'jvgrootveld/telescope-zoxide',
    { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
    'nvim-treesitter/nvim-treesitter-context',
    'ThePrimeagen/harpoon',
    'mbbill/undotree',
    { 'VonHeikemen/lsp-zero.nvim',
        dependencies = {
            -- LSP Support
            { 'neovim/nvim-lspconfig' },
            { 'williamboman/mason.nvim' },
            { 'williamboman/mason-lspconfig.nvim' },
            -- Autocompletion
            { 'hrsh7th/nvim-cmp' },
            { 'hrsh7th/cmp-buffer' },
            { 'hrsh7th/cmp-path' },
            { 'saadparwaiz1/cmp_luasnip' },
            { 'hrsh7th/cmp-nvim-lsp' },
            { 'hrsh7th/cmp-nvim-lua' },
            -- Snippets
            { 'L3MON4D3/LuaSnip' },
            { 'rafamadriz/friendly-snippets' },
        }
    },

    'lukas-reineke/indent-blankline.nvim',

    'numToStr/Comment.nvim',

    'ggandor/leap.nvim',

    'windwp/nvim-autopairs',

    'folke/twilight.nvim',

    'folke/zen-mode.nvim',

    { 'nvim-lualine/lualine.nvim',
        dependencies = { 'nvim-tree/nvim-web-devicons' },
    },

    'onsails/lspkind-nvim',

    'lewis6991/gitsigns.nvim',

    'akinsho/toggleterm.nvim',

    'kylechui/nvim-surround',

    'epwalsh/obsidian.nvim',


    'lewis6991/impatient.nvim',
    'rcarriga/nvim-notify',

    { 'iamcco/markdown-preview.nvim',
        build = function() vim.fn["mkdp#util#install"]() end
    },

    'zbirenbaum/copilot.lua'
}
