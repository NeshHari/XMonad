-- luacheck: globals vim
local ok_notify, notify = pcall(require, "notify")
if not ok_notify then
	return
end

notify.setup({
	stages = "fade_in_slide_out",
	timeout = 3000,
	background_colour = "#050508",
})

vim.notify = notify
