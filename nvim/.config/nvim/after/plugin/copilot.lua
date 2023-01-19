require('copilot').setup({
    panel = {
        enabled = true,
        auto_refresh = true,
        keymap = {
            jump_prev = "[[",
            jump_next = "]]",
            accept = "<CR>",
            refresh = "gr",
            open = "<M-CR>"
        },
    },
    suggestion = {
        enabled = true,
        auto_trigger = true,
        debounce = 75,
        keymap = {
            accept = "<C-f>",
            accept_word = false,
            accept_line = false,
            next = "<C-n>",
            prev = "<C-p>",
            dismiss = "<C-]>",
        },
    },
    filetypes = {
        yaml = true,
        markdown = true,
        help = false,
        gitcommit = false,
        gitrebase = false,
        hgcommit = false,
        svn = false,
        cvs = false,
        ["."] = false,
    },
    copilot_node_command = 'node', -- Node.js version must be > 16.x
    server_opts_overrides = {},
})

local cmp = require('cmp')
cmp.event:on("menu_opened", function()
    vim.b.copilot_suggestion_hidden = true
end)

cmp.event:on("menu_closed", function()
    vim.b.copilot_suggestion_hidden = false
end)

vim.keymap.set({ "n" }, "<C-[>", "<cmd>lua require('copilot.suggestion').toggle_auto_trigger()<CR>",
    { noremap = true, silent = true })
