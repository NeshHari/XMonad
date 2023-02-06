local ok_colorizer, colorizer = pcall(require, "colorizer")
if not ok_colorizer then
	return
end

colorizer.setup()
