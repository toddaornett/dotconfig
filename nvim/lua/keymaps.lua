-- Shorte function name
local keymap = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }
local term_opts = { silent = true }

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
keymap('n', '<leader>ff', ':lua require"telescope.builtin".find_files()<CR>', opts)
keymap('n', '<leader>fs', ':lua require"telescope.builtin".live_grep()<CR>', opts)
keymap('n', '<leader>fh', ':lua require"telescope.builtin".help_tags()<CR>', opts)
keymap('n', '<leader>fb', ':lua require"telescope.builtin".buffers()<CR>', opts)

-- Markdown preview
keymap('n', '<C-p>', '<Plug>MarkdownPreview', opts)

-- Text-case
keymap('n', 'gau', ':lua require"textcase".current_word("to_upper_case")<CR><CR>', opts)
keymap('n', 'gal', ':lua require"textcase".current_word("to_lower_case")<CR><CR>', opts)
keymap('n', 'gas', ':lua require"textcase".current_word("to_snake_case")<CR><CR>', opts)
keymap('n', 'gad', ':lua require"textcase".current_word("to_dash_case")<CR><CR>', opts)
keymap('n', 'gan', ':lua require"textcase".current_word("to_constant_case")<CR><CR>', opts)
keymap('n', 'gad', ':lua require"textcase".current_word("to_dot_case")<CR><CR>', opts)
keymap('n', 'gaa', ':lua require"textcase".current_word("to_phrase_case")<CR><CR>', opts)
keymap('n', 'gac', ':lua require"textcase".current_word("to_camel_case")<CR><CR>', opts)
keymap('n', 'gap', ':lua require"textcase".current_word("to_pascal_case")<CR><CR>', opts)
keymap('n', 'gat', ':lua require"textcase".current_word("to_title_case")<CR><CR>', opts)
keymap('n', 'gaf', ':lua require"textcase".current_word("to_path_case")<CR><CR>', opts)

keymap('n', 'gaU', ':lua require"textcase".lsp_rename("to_upper_case")<CR><CR>', opts)
keymap('n', 'gaL', ':lua require"textcase".lsp_rename("to_lower_case")<CR><CR>', opts)
keymap('n', 'gaS', ':lua require"textcase".lsp_rename("to_snake_case")<CR><CR>', opts)
keymap('n', 'gaD', ':lua require"textcase".lsp_rename("to_dash_case")<CR><CR>', opts)
keymap('n', 'gaN', ':lua require"textcase".lsp_rename("to_constant_case")<CR><CR>', opts)
keymap('n', 'gaD', ':lua require"textcase".lsp_rename("to_dot_case()<CR><CR>', opts)
keymap('n', 'gaA', ':lua require"textcase".lsp_rename("to_phrase_case")<CR><CR>', opts)
keymap('n', 'gaC', ':lua require"textcase".lsp_rename("to_camel_case")<CR><CR>', opts)
keymap('n', 'gaP', ':lua require"textcase".lsp_rename("to_pascal_case")<CR><CR>', opts)
keymap('n', 'gaT', ':lua require"textcase".lsp_rename("to_title_case")<CR><CR>', opts)
keymap('n', 'gaF', ':lua require"textcase".lsp_rename("to_path_case")<CR><CR>', opts)

keymap('n', 'geu', ':lua require"textcase".operator("to_upper_case")<CR><CR>', opts)
keymap('n', 'gel', ':lua require"textcase".operator("to_lower_case")<CR><CR>', opts)
keymap('n', 'ges', ':lua require"textcase".operator("to_snake_case")<CR><CR>', opts)
keymap('n', 'ged', ':lua require"textcase".operator("to_dash_case")<CR><CR>', opts)
keymap('n', 'gen', ':lua require"textcase".operator("to_constant_case")<CR><CR>', opts)
keymap('n', 'ged', ':lua require"textcase".operator("to_dot_case")<CR><CR>', opts)
keymap('n', 'gea', ':lua require"textcase".operator("to_phrase_case")<CR><CR>', opts)
keymap('n', 'gec', ':lua require"textcase".operator("to_camel_case")<CR><CR>', opts)
keymap('n', 'gep', ':lua require"textcase".operator("to_pascal_case")<CR><CR>', opts)
keymap('n', 'get', ':lua require"textcase".operator("to_title_case")<CR><CR>', opts)
keymap('n', 'gef', ':lua require"textcase".operator("to_path_case")<CR><CR>', opts)

-- Launch terminal
keymap('n', '<leader>T', ':terminal<CR>', opts)

-- Terminal mode --
keymap('t', '<C-h>', '<C-\\><C-n><C-w>h', term_opts)
keymap('t', '<C-j>', '<C-\\><C-n><C-w>j', term_opts)
keymap('t', '<C-k>', '<C-\\><C-n><C-w>k', term_opts)
keymap('t', '<C-l>', '<C-\\><C-n><C-w>l', term_opts)
keymap('t', '<Esc>', '<C-\\><C-n>', term_opts)
keymap('t', '<C-d>', '<C-\\><C-n>:q!<CR>', term_opts)
