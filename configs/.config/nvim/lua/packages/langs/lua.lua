return {
    {
        -- Add endings to things like function and ifs
        'tpope/vim-endwise',
    },
    {
        'folke/lazydev.nvim',
        lazy = true,
        ft = 'lua',
        opts = {
            library = {
                { path = '${3rd}/luv/library', words = { 'vim%.uv' } },
                { path = 'LazyVim', words = { 'LazyVim' } },
            },
        },
    },
}
