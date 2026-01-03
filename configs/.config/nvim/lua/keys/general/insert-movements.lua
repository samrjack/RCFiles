-- check :h map-listing for info on effected modes

-- Move by character amounts
vim.keymap.set({ '!', 't' }, '<C-h>', '<Left>', { desc = "Move one character left" })
vim.keymap.set({ '!', 't' }, '<C-j>', '<Down>', { desc = "Move one character down" })
vim.keymap.set({ '!', 't' }, '<C-k>', '<Up>', { desc = "Move one character up" })
vim.keymap.set({ '!', 't' }, '<C-l>', '<Right>', { desc = "Move one character right" })

-- Move by word amounts
vim.keymap.set({ '!', 't' }, '<M-h>', '<C-Left>', { desc = "Move one word left" })
vim.keymap.set({ '!', 't' }, '<M-j>', '<C-o>}', { desc = "Move one paragraph down" })
vim.keymap.set({ '!', 't' }, '<M-k>', '<C-o>{', { desc = "Move one paragraph up" })
vim.keymap.set({ '!', 't' }, '<M-l>', '<C-Right>', { desc = "Move one word right" })
