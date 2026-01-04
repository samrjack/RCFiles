local function enable_folding()
    vim.opt.foldenable = true
    vim.keymap.set('n', '<tab>', 'za', { desc = 'Fold/Unfold' })
end

vim.opt.foldmethod = 'indent' -- will have to check if there's a way to enable this with lsp
vim.opt.foldnestmax = 100 -- prevent too many folds
vim.opt.foldlevelstart = 20 -- start with pretty much everything open
enable_folding()

vim.keymap.set('n', '<leader>cf', function()
    if vim.opt.foldenable:get() then
        vim.opt.foldenable = false
    else
        enable_folding()
    end
end, { desc = 'Toggle folding' })
