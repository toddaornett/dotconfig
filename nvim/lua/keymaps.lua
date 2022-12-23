-- Shorte function name
local keymap = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }

-- Press jk fast to enter normal mode
keymap('i', 'jk', '<Esc>', opts)

-- Normal mode --

-- Toggle nvim-tree
keymap('n', '<leader>n', [[:NvimTreeToggle<CR>]], opts)

-- Change configuration
keymap('n', '<leader>ei', ':e ~/.config/nvim/init.lua<CR>', opts)
keymap('n', '<leader>ek', ':e ~/.config/nvim/lua/keymaps.lua<CR>', opts)
keymap('n', '<leader>ep', ':e ~/.config/nvim/lua/packer-plugins.lua<CR>', opts)
keymap('n', '<leader>es', ':e ~/.config/nvim/lua/settings.lua<CR>', opts)

-- Better window navigation
keymap('n', '<C-h>', '<C-w>h', opts)
keymap('n', '<C-j>', '<C-w>j', opts)
keymap('n', '<C-k>', '<C-w>k', opts)
keymap('n', '<C-l>', '<C-w>l', opts)

-- Resize with arrows
keymap('n', '<C-Up>', ':resize -2<CR>', opts)
keymap('n', '<C-Down>', ':resize +2<CR>', opts)
keymap('n', '<C-Left>', ':vertical resize -2<CR>', opts)
keymap('n', '<C-Right>', ':vertical resize +2<CR>', opts)

-- Navigate buffers
keymap('n', '<S-l>', ':bnext<CR>', opts)
keymap('n', '<S-h>', ':bprevious<CR>', opts)

-- Clear search highlight until next search
keymap('n', '<cr>', ':nohlsearch<CR>', opts)

-- Telescope
keymap('n', '<C-p>', ':lua require"telescope.builtin".find_files()<CR>', opts)
keymap('n', '<leader>fs', ':lua require"telescope.builtin".live_grep()<CR>', opts)
keymap('n', '<leader>fh', ':lua require"telescope.builtin".help_tags()<CR>', opts)
keymap('n', '<leader>fb', ':lua require"telescope.builtin".buffers()<CR>', opts)
