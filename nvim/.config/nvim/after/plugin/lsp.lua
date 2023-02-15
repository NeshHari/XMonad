-- luacheck: globals vim

local ok_lsp, lsp = pcall(require, "lsp-zero")
if not ok_lsp then
	return
end
lsp.preset("recommended")
lsp.nvim_workspace()
lsp.setup()

local opts = { noremap = true, silent = true }
local key = vim.keymap.set

local ok_lspconf, myLsp = pcall(require, "lspconfig")
if not ok_lspconf then
	return
end

local ok_mason, mason = pcall(require, "mason")
if not ok_mason then
	return
end

mason.setup()

local ok_masonlspconf, masonlspconf = pcall(require, "mason-lspconfig")
if not ok_masonlspconf then
	return
end

local lsp_services = { "bashls", "hls", "jdtls", "marksman", "lua_ls", "pyright" }

masonlspconf.setup({
	ensure_installed = lsp_services,
})

local ok_sig, lspsig = pcall(require, "lsp_signature")
if not ok_sig then
	return
end

lspsig.setup({
	bind = false,
	handler_opts = {
		border = "rounded",
	},
})

local on_attach = function(bufnr)
	-- Enable completion triggered by <c-x><c-o>
	vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

	-- Mappings.
	local bufopts = { noremap = true, silent = true, buffer = bufnr }
	key("n", "<C-k>", vim.lsp.buf.signature_help, bufopts)
	key("n", "<leader>wa", vim.lsp.buf.add_workspace_folder, bufopts)
	key("n", "<leader>wr", vim.lsp.buf.remove_workspace_folder, bufopts)
	key("n", "<leader>wl", function()
		print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
	end, bufopts)
	lspsig.on_attach(bufnr)
end

local capabilities = require("cmp_nvim_lsp").default_capabilities()

for _, service in ipairs(lsp_services) do
	myLsp[service] = { on_attach = on_attach }
	myLsp[service] = { capabilities = capabilities }
	if myLsp[service] == "lua_ls" then
		myLsp[service] = { settings = { Lua = { diagnostics = { globals = { "vim" } } } } }
	end
end

require("lspkind").init()

local lspkind = require("lspkind")
require("cmp").setup({
	formatting = {
		format = lspkind.cmp_format({
			mode = "symbol",
			maxwidth = 50,
			ellipsis_char = "...",
		}),
	},
})

local ok_cmp, cmp = pcall(require, "cmp")
if not ok_cmp then
	return
end

cmp.setup({
	snippet = {
		expand = function(args)
			require("luasnip").lsp_expand(args.body)
		end,
	},
	window = {
		completion = cmp.config.window.bordered(),
		documentation = cmp.config.window.bordered(),
	},
	mapping = cmp.mapping.preset.insert({
		["<C-b>"] = cmp.mapping.scroll_docs(-4),
		["<C-f>"] = cmp.mapping.scroll_docs(4),
		["<C-Space>"] = cmp.mapping.complete(),
		["<C-e>"] = cmp.mapping.abort(),
		["<CR>"] = cmp.mapping.confirm({ select = true }),
	}),
	sources = cmp.config.sources({
		{ name = "copilot" },
		{ name = "nvim_lsp" },
		{ name = "luasnip" },
		{ name = "buffer" },
		{ name = "obsidian" },
	}),
	formatting = {
		format = lspkind.cmp_format({
			mode = "symbol",
			maxwidth = 50,
			ellipsis_char = "...",
		}),
	},
})

local ok_saga, saga = pcall(require, "lspsaga")
if not ok_saga then
	return
end

saga.setup({
	ui = {
		colors = require("catppuccin.groups.integrations.lsp_saga").custom_colors(),
		kind = require("catppuccin.groups.integrations.lsp_saga").custom_kind(),
	},
})

key("n", "gh", "<cmd>Lspsaga lsp_finder<CR>")
key({ "n", "v" }, "<leader>ca", "<cmd>Lspsaga code_action<CR>")
key("n", "gr", "<cmd>Lspsaga rename<CR>")
key("n", "gd", "<cmd>Lspsaga goto_definition<CR>")
key("n", "gp", "<cmd>Lspsaga peek_definition<CR>")
key("n", "<leader>sl", "<cmd>Lspsaga show_line_diagnostics<CR>")
key("n", "<leader>sc", "<cmd>Lspsaga show_cursor_diagnostics<CR>")
key("n", "<leader>sb", "<cmd>Lspsaga show_buf_diagnostics<CR>")
key("n", "[e", "<cmd>Lspsaga diagnostic_jump_prev<CR>")
key("n", "]e", "<cmd>Lspsaga diagnostic_jump_next<CR>")
key("n", "[E", function()
	require("lspsaga.diagnostic"):goto_prev({ severity = vim.diagnostic.severity.ERROR })
end)
key("n", "]E", function()
	require("lspsaga.diagnostic"):goto_next({ severity = vim.diagnostic.severity.ERROR })
end)
key("n", "<leader>o", "<cmd>Lspsaga outline<CR>")
key("n", "<leader>k", "<cmd>Lspsaga hover_doc<CR>")
key("n", "<leader>K", "<cmd>Lspsaga hover_doc ++keep<CR>")
key("n", "<leader>ci", "<cmd>Lspsaga incoming_calls<CR>")
key("n", "<leader>co", "<cmd>Lspsaga outgoing_calls<CR>")

local ok_trouble, trouble = pcall(require, "trouble")
if not ok_trouble then
	return
end

trouble.setup()

key("n", "<leader>xx", "<cmd>TroubleToggle<cr>", opts)
key("n", "<leader>xw", "<cmd>TroubleToggle workspace_diagnostics<cr>", opts)
key("n", "<leader>xd", "<cmd>TroubleToggle document_diagnostics<cr>", opts)
key("n", "<leader>xl", "<cmd>TroubleToggle loclist<cr>", opts)
key("n", "<leader>xq", "<cmd>TroubleToggle quickfix<cr>", opts)
key("n", "<leader>gr", "<cmd>TroubleToggle lsp_references<cr>", opts)
