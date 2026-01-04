local group_id = vim.api.nvim_create_augroup('treesitter', {})

vim.api.nvim_create_autocmd('BufWinEnter', {
    group = group_id,
    desc = 'Set folding style to treesitter when available.',
    callback = function()
        if require('nvim-treesitter.parsers').has_parser() then
            vim.wo.foldmethod = 'expr'
            vim.wo.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
        else
            vim.wo.foldmethod = 'indent'
        end
    end,
})
