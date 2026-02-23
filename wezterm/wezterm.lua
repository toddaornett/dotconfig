-- Wezterm Configuration
local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.initial_cols = 120
config.initial_rows = 28

config.font_size = 16
config.color_scheme = 'Bamboo'

-- HarfBuzz required for OpenType features like slashed zero.
-- Use only the Fira Code example from WezTerm docs: { 'zero' }.
config.font_shaper = 'Harfbuzz'
config.harfbuzz_features = { 'zero' }

-- Primary first; fallback used for missing glyphs (e.g. Nerd icons).
-- If zero still dotted, swap order: put 'JetBrains Mono Nerd Font' first to test.
config.font = wezterm.font_with_fallback {
  { family = 'FiraCode Nerd Font', weight = 'Regular' },
  'JetBrains Mono Nerd Font',
}


return config
