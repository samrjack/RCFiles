-- Plugins for working with git

DescribeKeys({
    { '<leader>g', group = 'Git' },
    { '<leader>gh', group = 'Hunks' },
}, { 'n', 'v' })

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
                delay = 10,
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
            {
                '<leader>gs',
                function()
                    require('gitsigns').stage_hunk({ vim.fn.line('.'), vim.fn.line('v') })
                end,
                mode = { 'v', 'n' },
                desc = 'Stage selection',
            },
            {
                '<leader>gS',
                function()
                    require('gitsigns').stage_buffer()
                end,
                mode = { 'n' },
                desc = 'Stage buffer',
            },
            {
                '<leader>gr',
                function()
                    require('gitsigns').reset_hunk({ vim.fn.line('.'), vim.fn.line('v') })
                end,
                mode = { 'v', 'n' },
                desc = 'Reset selection',
            },
            {
                '<leader>gR',
                function()
                    require('gitsigns').reset_buffer()
                end,
                mode = { 'n' },
                desc = 'Stage buffer',
            },
            {
                '<leader>ghp',
                function()
                    require('gitsigns').preview_hunk()
                end,
                mode = { 'n' },
                desc = 'preview',
            },
            {
                '<leader>ghs',
                function()
                    require('gitsigns').stage_hunk()
                end,
                mode = { 'n' },
                desc = 'preview',
            },
            {
                '<leader>ghr',
                function()
                    require('gitsigns').reset_hunk()
                end,
                mode = { 'n' },
                desc = 'preview',
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
        opts = {
            mappings = {
                popup = {
                    ['F'] = 'PullPopup',
                    ['p'] = 'PushPopup',
                    ['P'] = 'PushPopup',
                },
            },
        },
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
    {
        'chrishrb/gx.nvim',
        lazy = true,
        dependencies = {
            'nvim-lua/plenary.nvim',
        },
        init = function()
            vim.g.netrw_nogx = 1
        end,
        opts = {},
        submodules = false,
        cmd = { 'Browse' },
        keys = {
            { 'gx', '<cmd>Browse<cr>', desc = 'Open in browser' },
            { '<leader>gx', '<cmd>Browse<cr>', desc = 'Open in browser' },
        },
    },
}
