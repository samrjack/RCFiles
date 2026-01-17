DescribeKeys({ '<leader>cu', group = 'Unstack' })
return {
    {
        'relf108/nvim-unstack',
        lazy = true,
        cmd = 'NvimUnstack',
        opts = {
            layout = 'floating',
            mapkey = false,
        },
        keys = {
            {
                '<leader>cuu',
                function()
                    require('nvim-unstack').unstack()
                end,
                mode = { 'v' },
                desc = 'Unstack selected',
            },
            {
                '<leader>cuc',
                function()
                    require('nvim-unstack').unstack_from_clipboard()
                end,
                desc = 'Unstack clipboard',
            },
            {
                '<leader>cut',
                function()
                    require('nvim-unstack').unstack_from_tmux()
                end,
                desc = 'Unstack tmux paste buffer',
            },
        },
    },
}
