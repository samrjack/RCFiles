-- This package removes white space at the end of lines when a file is saved

return {
    'mcauley-penney/tidy.nvim',
    lazy = true,
    event = 'BufWritePre',
    opts = {},
    keys = {
        {
            '<leader>tt',
            function()
                require('tidy').toggle()
            end,
            desc = 'Tidy trailing spaces',
        },
    },
}
