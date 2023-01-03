-- Run PackerSync when this file is written to
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost packer.lua source <afile> | PackerSync
  augroup end
]])

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.0',
        requires = { { 'nvim-lua/plenary.nvim' } }
    }

    use { "nvim-telescope/telescope-file-browser.nvim" }

    use { 'catppuccin/nvim', as = "catppuccin" }

    use('nvim-treesitter/nvim-treesitter', { run = ':TSUpdate' })
    use ('nvim-treesitter/nvim-treesitter-context')

    use('ThePrimeagen/harpoon')
    use('mbbill/undotree')

    use {
        'VonHeikemen/lsp-zero.nvim',
        requires = {
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
    }

    use {
        "ray-x/lsp_signature.nvim",
    }

    use {
        'akinsho/bufferline.nvim', tag = "v3.*", requires = 'nvim-tree/nvim-web-devicons',
        after = "catppuccin",
    }

    use { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }

    use { 'nvim-lua/popup.nvim' }

    use { 'jvgrootveld/telescope-zoxide' }

    use { 'glepnir/dashboard-nvim' }

    use "lukas-reineke/indent-blankline.nvim"

    use {
        'numToStr/Comment.nvim',
        config = function()
            require('Comment').setup()
        end
    }

    use { 'ggandor/leap.nvim' }

    use {
        "windwp/nvim-autopairs",
        config = function() require("nvim-autopairs").setup {} end
    }

    use {
        "folke/twilight.nvim",
        config = function()
            require("twilight").setup {
            }
        end
    }

    use {
        "folke/zen-mode.nvim",
        config = function()
            require("zen-mode").setup {
                plugins = {
                    twilight = {
                        enabled = true,
                    }
                }
            }
        end
    }

    use {
        'nvim-lualine/lualine.nvim',
        requires = { 'kyazdani42/nvim-web-devicons', opt = true }
    }

    use 'onsails/lspkind-nvim'

    use {
        'lewis6991/gitsigns.nvim',
        config = function()
            require('gitsigns').setup()
        end
    }

    use { "akinsho/toggleterm.nvim", tag = '*' }

end)
