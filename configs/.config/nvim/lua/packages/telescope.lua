return {
    'nvim-telescope/telescope.nvim',
    dependencies = {
        'nvim-lua/plenary.nvim',
    },
    lazy = true,
    opts = {
        defaults = {
            mappings = {
                i = {
                    ['<C-h>'] = 'which_key',
                },
                n = {
                    ['q'] = 'close',
                },
            },
        },
    },
    keys = {
        {
            '<leader>fa',
            function()
                require('telescope.builtin').builtin()
            end,
            desc = 'All pickers',
        },
        {
            '<leader>fb',
            function()
                require('telescope.builtin').buffers()
            end,
            desc = 'Buffer',
        },
        {
            '<leaderfc',
            function()
                require('telescope.builtin').colorscheme({ enable_preview = true })
            end,
            desc = 'Colorscheme',
        },
        {
            '<leader>ff',
            function()
                require('telescope.builtin').find_files()
            end,
            desc = 'Files',
        },
        {
            '<leader>fg',
            function()
                require('telescope.builtin').live_grep()
            end,
            desc = 'Grep files',
        },
        {
            '<leader>fh',
            function()
                require('telescope.builtin').help_tags()
            end,
            desc = 'Help tag',
        },
        {
            '<leader>fm',
            function()
                require('telescope.builtin').man_pages()
            end,
            desc = 'Man page',
        },
        {
            '<leader>fM',
            function()
                require('telescope.builtin').marks()
            end,
            desc = 'Marks',
        },
        {
            '<leader>fp',
            function()
                require('telescope.builtin').git_files()
            end,
            desc = 'Project files',
        },
        {
            '<leader>ft',
            function()
                require('telescope.builtin').treesitter()
            end,
            desc = 'Treesitter',
        },
        {
            '<leader>ha',
            function()
                require('telescope.builtin').autocommands()
            end,
            desc = 'Describe variable',
        },
        {
            '<leader>hc',
            function()
                require('telescope.builtin').colorscheme()
            end,
            desc = 'Colorschemes',
        },
        {
            '<leader>hf',
            function()
                require('telescope.builtin').filetypes()
            end,
            desc = 'Change filetype',
        },
        {
            '<leader>hv',
            function()
                require('telescope.builtin').vim_options()
            end,
            desc = 'Describe variable',
        },
        {
            '<leader>sc',
            function()
                require('telescope.builtin').command_history()
            end,
            desc = 'Command history',
        },
        {
            '<leader>ss',
            function()
                require('telescope.builtin').search_history()
            end,
            desc = 'Search history',
        },
    },
}
