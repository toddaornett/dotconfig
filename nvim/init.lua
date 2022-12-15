-- global options
vim.g.mapleader = ' '

-- disable netrw at the very start of your init.lua (strongly advised)
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

require('settings')
require('packer-plugins')
require("mason").setup()
require('kommentary.config').use_extended_mappings()
require("nvim-tree").setup({
  sort_by = "case_sensitive",
  view = {
    adaptive_size = true,
    side = "right",
    mappings = {
      list = {
        { key = "u", action = "dir_up" },
      },
    },
  },
  renderer = {
    group_empty = true,
  },
  filters = {
    dotfiles = true,
  },
})
require('keymaps')
require("nvim-autopairs")
require('telescope').setup {
  defaults = {
    mappings = { i = {
      ['<C-p>'] = require('telescope.actions.layout').toggle_preview}
    },
    preview = {
      hide_on_startup = false
    }
  },
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    }
  }
}
require('telescope').load_extension('fzf')
