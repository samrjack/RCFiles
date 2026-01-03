vim.keymap.set('n', '<leader>sw', [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], {desc = 'Replace word'})
-- Uses lare W instead of small w
vim.keymap.set('n', '<leader>sW', [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], {desc = 'Replace Word'})
