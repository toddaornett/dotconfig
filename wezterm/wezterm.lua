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
config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0', 'zero' }

-- Primary first; fallback used for missing glyphs (e.g. Nerd icons).
-- If zero still dotted, swap order: put 'JetBrains Mono Nerd Font' first to test.
config.font = wezterm.font_with_fallback {
  { family = 'FiraCode Nerd Font', weight = 'Regular' },
  'JetBrains Mono Nerd Font',
}

-- Remap Option + ¥ to \
config.keys = {
  {
    key = 'raw:93', -- '¥'
    mods = 'OPT',
    action = wezterm.action.SendString('\\'),
  },
}

-- Start from Wezterm's built-in key tables so we keep all default
-- copy-mode bindings (h/j/k/l, v, y, etc.) and just add our own on top.
local default_tables = wezterm.gui.default_key_tables()
local copy_mode = default_tables.copy_mode or {}

table.insert(copy_mode, {
  key = 'G',
  mods = 'NONE',
  action = wezterm.action.CopyMode 'MoveToScrollbackBottom',
})

config.key_tables = {
  copy_mode = copy_mode,
}

return config
