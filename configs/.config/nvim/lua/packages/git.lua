-- Plugins for working with git

return {
    {
        'tpope/vim-fugitive',
        lazy = true,
        keys = {
            { '<leader>gG', vim.cmd.Git, desc = 'futitive' },
            {
                '<leader>gB',
                function()
                    if vim.bo.filetype == 'fugitiveblame' then
                        vim.api.nvim_win_close(0, true)
                    else
                        vim.cmd.Git('blame')
                    end
                end,
                desc = 'toggle sideline blame',
            },
        },
    },
    {
        'lewis6991/gitsigns.nvim',
        lazy = true,
        event = 'BufRead',
        opts = {
            current_line_blame_formatter = ' <author_time:%Y-%m-%d %H:%M> (<author_time:%R>) • <summary> • <author> • [<abbrev_sha>]',
            current_line_blame_opts = {
                delay = 400,
            },
        },
        keys = {
            {
                '<leader>gb',
                function()
                    vim.cmd.Gitsigns('toggle_current_line_blame')
                end,
                desc = 'Toggle inline blame',
            },
            {
                '<leader>gd',
                function()
                    require('gitsigns').toggle_deleted()
                end,
                desc = 'Toggle deleted',
            },
        },
    },
    {
        'NeogitOrg/neogit',
        dependencies = {
            'nvim-lua/plenary.nvim',
            'sindrets/diffview.nvim',
            'nvim-telescope/telescope.nvim',
        },
        lazy = true,
        config = true,
        keys = {
            {
                '<leader>gg',
                function()
                    require('neogit').open()
                end,
                desc = 'Git status',
            },
        },
    },
    {
        'rhysd/git-messenger.vim',
        lazy = true,
        config = function()
            vim.g.git_messenger_always_into_popup = true
            vim.g.git_messenger_floating_win_opts = { border = 'double' }
            vim.g.git_messenger_popup_content_margins = false
        end,
        keys = {
            { '<leader>gm', vim.cmd.GitMessenger, desc = 'View git commit message' },
        },
    },
}
