-- Remap Ctrl-i's functionality since Ctrl-i is tied to tab
vim.keymap.set('n', '<C-p>', '<C-i>', { desc = 'Next Jumplist' })
vim.keymap.set('n', ']j', '<C-i>', { desc = 'Next Jumplist' })
vim.keymap.set('n', '[j', '<C-o>', { desc = 'Previous Jumplist' })
vim.keymap.set('n', '<M-o>', '<C-i>', { desc = 'Jump forward' })
