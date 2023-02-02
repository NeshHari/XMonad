local dashboard = require("alpha.themes.dashboard")

-- Banner
local darkarts = {
	"████████╗██╗  ██╗███████╗    ██████╗  █████╗ ██████╗ ██╗  ██╗     █████╗ ██████╗ ████████╗███████╗",
	"╚══██╔══╝██║  ██║██╔════╝    ██╔══██╗██╔══██╗██╔══██╗██║ ██╔╝    ██╔══██╗██╔══██╗╚══██╔══╝██╔════╝",
	"   ██║   ███████║█████╗      ██║  ██║███████║██████╔╝█████╔╝     ███████║██████╔╝   ██║   ███████╗",
	"   ██║   ██╔══██║██╔══╝      ██║  ██║██╔══██║██╔══██╗██╔═██╗     ██╔══██║██╔══██╗   ██║   ╚════██║",
	"   ██║   ██║  ██║███████╗    ██████╔╝██║  ██║██║  ██║██║  ██╗    ██║  ██║██║  ██║   ██║   ███████║",
	"   ╚═╝   ╚═╝  ╚═╝╚══════╝    ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝    ╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝   ╚══════╝",
	"                                                                                                  ",
}

dashboard.section.header.val = darkarts

dashboard.section.buttons.val = {
	dashboard.button("SPC f f", "  Find File", ":Telescope find_files find_command=rg,--hidden,--files<CR>"),
	dashboard.button("SPC f b", "  File Browser", ":Telescope file_browser<CR>"),
	dashboard.button("SPC f w", "  Find Word", ":Telescope live_grep<CR>"),
	dashboard.button("SPC f d", "  Dotfiles", ":cd $HOME/dotfiles/ | Telescope file_browser<CR>"),
	dashboard.button("SPC f h", "  Help", ":Telescope help_tags<CR>"),
	dashboard.button("q", "  Get Me Outta Here", ":qa<CR>"),
}

require("alpha").setup(dashboard.config)
