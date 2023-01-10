local builtin = require('telescope.builtin')

local z_utils = require("telescope._extensions.zoxide.utils")

local telescope = require("telescope")
telescope.setup {
    defaults = {
        previewer = true;
    },
    extensions = {
        fzf = {
            fuzzy = true, -- false will only do exact matching
            override_generic_sorter = true, -- override the generic sorter
            override_file_sorter = true, -- override the file sorter
            case_mode = "smart_case", -- or "ignore_case" or "respect_case"
        },
        zoxide = {
            prompt_title = "[ Whatcha lookin' 4? ]",
            -- Zoxide list command with score
            list_command = "zoxide query -ls",
            mappings = {
                default = {
                    action = function(selection)
                        vim.cmd("cd " .. selection.path)
                    end,
                    after_action = function(selection)
                        print("Directory changed to " .. selection.path)
                    end
                },
                ["<C-s>"] = { action = z_utils.create_basic_command("split") },
                ["<C-v>"] = { action = z_utils.create_basic_command("vsplit") },
                ["<C-e>"] = { action = z_utils.create_basic_command("edit") },
                ["<C-b>"] = {
                    keepinsert = true,
                    action = function(selection)
                        builtin.file_browser({ cwd = selection.path })
                    end
                },
                ["<C-f>"] = {
                    keepinsert = true,
                    action = function(selection)
                        builtin.find_files({ cwd = selection.path })
                    end
                }
            }
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
    }
}


telescope.load_extension('fzf')
telescope.load_extension('zoxide')
telescope.load_extension('file_browser')

vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<leader>fw', builtin.live_grep, {})
vim.keymap.set("n", "<leader>fb", telescope.extensions.file_browser.file_browser)
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
vim.keymap.set('n', '<leader>bf', builtin.buffers, {})
