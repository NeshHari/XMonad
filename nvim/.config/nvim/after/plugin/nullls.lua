local null_ls = require("null-ls")

local formatting = null_ls.builtins.formatting
local diagnostics = null_ls.builtins.diagnostics
local completion = null_ls.builtins.completion

-- formatters can be installed via Mason
null_ls.setup({
	sources = {
		formatting.stylua,
		formatting.shfmt,
		formatting.prettierd,
		formatting.black,
		--formatting.fourmolu, <-- (ormolu alternative)
		diagnostics.markdownlint,
		diagnostics.luacheck,
		completion.spell,
	},
})
