doReload = false
function reloadConfig(files)
	for _, file in pairs(files) do
		if file:sub(-4) == ".lua" then
			doReload = true
		end
	end
	if doReload then
		hs.reload()
	end
end
myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.alert.show("Config loaded")

hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration:start()

hs.hotkey.bind({ "command" }, "escape", function()
	hs.execute("open /Applications/Alacritty.app")
end)

hs.hotkey.bind({ "command", "alt" }, "b", function()
	hs.execute("open /Applications/Brave Browser.app")
end)

hs.hotkey.bind({ "command", "alt" }, "c", function()
	hs.execute("open /Applications/Google Chrome.app")
end)

hs.hotkey.bind({ "command", "alt" }, "d", function()
	hs.execute("open /Applications/Docker.app")
end)

hs.hotkey.bind({ "command", "alt" }, "f", function()
	hs.execute("open /Applications/Firefox.app")
end)

hs.hotkey.bind({ "command", "alt" }, "m", function()
	hs.execute("open /Applications/Min.app")
end)

hs.hotkey.bind({ "command", "alt" }, "o", function()
	hs.execute("open /Applications/Microsoft Outlook.app")
end)

hs.hotkey.bind({ "command", "alt" }, "s", function()
	hs.execute("open /Applications/Safari.app")
end)

hs.hotkey.bind({ "command", "alt" }, "z", function()
	hs.execute("open /Applications/zoom.us.app")
end)
