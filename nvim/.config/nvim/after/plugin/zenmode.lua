require("zen-mode").setup {
    window = {
        backdrop = 1,
        width = 50,
        height = 0.5,
        options = {
            signcolumn = "no",
            number = false,
            relativenumber = false,
            cursorline = false,
            cursorcolumn = false,
            foldcolumn = "0",
            list = false,
        },
    },
    plugins = {
        options = {
            enabled = true,
            ruler = false,
            showcmd = false,
        },
        twilight = { enabled = false },
        gitsigns = { enabled = false },
    },
}
