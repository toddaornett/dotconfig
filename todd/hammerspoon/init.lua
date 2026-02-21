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

local terminal_map = {
  alacritty = "org.alacritty",
  ghostty   = "com.mitchellh.ghostty",
  kitty     = "net.kovidgoyal.kitty",
  iterm     = "com.googlecode.iterm2",
  iterm2    = "com.googlecode.iterm2",
  terminal  = "com.apple.Terminal",
  wezterm   = "com.github.wez.wezterm",
  warp      = "dev.warp.Warp-Stable",
}

local function launch_terminal()
  local choice = hs.settings.get("hammerspoon_terminal") or "terminal"
  choice = string.lower(choice)
  local bundleID = terminal_map[choice] or "com.apple.Terminal"
  hs.application.launchOrFocusByBundleID(bundleID)
end

myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.alert.show("Config loaded")

hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration:start()

local ipc_ok, _ = pcall(require, "hs.ipc")
if not ipc_ok then
  hs.alert.show("Hammerspoon IPC module unavailable")
end

local function hs_cli_installed()
  local candidates = {
    "/opt/homebrew/bin/hs",
    "/usr/local/bin/hs",
    "/usr/bin/hs",
    "/Applications/Hammerspoon.app/Contents/Resources/hs",
  }

  for _, path in ipairs(candidates) do
    local attrs = hs.fs.attributes(path)
    if attrs and (attrs.mode == "file" or attrs.mode == "link") then
      return true
    end
  end

  local which = hs.execute("command -v hs 2>/dev/null")
  return which and which:match("%S") ~= nil
end

if ipc_ok and not hs_cli_installed() then
  hs.ipc.cliInstall()
end

hs.hotkey.bind({ "command" }, "escape", launch_terminal)

hs.hotkey.bind({ "command", "alt" }, "b", function()
	hs.execute("open /Applications/Brave Browser.app")
end)

hs.hotkey.bind({ "command", "alt" }, "c", function()
	hs.execute("open /Applications/Google Chrome.app")
end)

hs.hotkey.bind({ "command", "alt" }, "e", function()
	hs.execute("open -a /Applications/Emacs.app")
end);

hs.hotkey.bind({ "command", "alt" }, "f", function()
	hs.execute("open /Applications/Firefox.app")
end)

hs.hotkey.bind({ "command", "alt" }, "m", function()
	hs.execute("open /Applications/Min.app")
end)

hs.hotkey.bind({ "command", "alt" }, "o", function()
	hs.execute("open /Applications/Microsoft Outlook.app")
end)

hs.hotkey.bind({ "command", "alt" }, "p", function()
	hs.execute("open /Applications/DBeaver.app")
end)

hs.hotkey.bind({ "command", "alt" }, "s", function()
	hs.execute("open /Applications/Safari.app")
end)

hs.hotkey.bind({ "command", "alt" }, "t", function()
	hs.execute("open /Applications/Microsoft Teams.app")
end)

hs.hotkey.bind({ "command", "alt" }, "z", function()
	hs.execute("open /Applications/zoom.us.app")
end)
