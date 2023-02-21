-- luacheck: globals vim
local ok_notify, notify = pcall(require, "notify")
if not ok_notify then
	return
end

notify.setup({
	stages = "fade_in_slide_out",
	timeout = 2000,
	background_colour = "#1e1e2e",
})

vim.notify = notify
