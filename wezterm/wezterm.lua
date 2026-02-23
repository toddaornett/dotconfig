-- Wezterm Configuration
local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.initial_cols = 120
config.initial_rows = 28

config.font_size = 16
config.color_scheme = 'Bamboo'

-- Specify your font and enable ligatures
config.font = wezterm.font('FiraCode Nerd Font')
-- Alternatively, using font_with_fallback
-- config.font = wezterm.font_with_fallback { 'FiraCode Nerd Font', 'JetBrains Mono' }

-- Optional: Fine-tune HarfBuzz features (e.g., enable all standard ligatures)
config.harfbuzz_features = { 'zero', 'calt=1', 'clig=1', 'liga=1' }


return config
