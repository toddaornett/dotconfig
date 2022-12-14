local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'
  -- Automatically set up your configuration after cloning packer.nvim

  use 'nvim-lua/popup.nvim'

  -- plenary library
  use 'nvim-lua/plenary.nvim'

  -- colorscheme
  use 'gruvbox-community/gruvbox'

  -- lsp setup
  use 'williamboman/mason.nvim'
  use 'williamboman/mason-lspconfig.nvim'

  -- file explorer
  use {
    'nvim-tree/nvim-tree.lua',
    requires = {
        'nvim-tree/nvim-web-devicons', -- optional, for file icons
    },
    tag = 'nightly' -- optional, updated every week. (see issue #1193)
  }

  -- miscellaneous code editing
  use 'b3nj5m1n/kommentary'
  use {
    "windwp/nvim-autopairs", config = function() require("nvim-autopairs").setup {} end
  }

  -- syntatic highlighting
  use 'nvim-treesitter/nvim-treesitter'
  use 'sheerun/vim-polyglot'
  use 'tjdevries/colorbuddy.nvim'
  use 'bkegley/gloombuddy'
  use {'prettier/vim-prettier', run = 'yarn install' }

  -- telescope
  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build' }

  use {
    'nvim-telescope/telescope.nvim', tag = '0.1.0',
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  -- Execute after all plugins
  if packer_bootstrap then
    require('packer').sync()
  end
end)
