local TREE_SITTER_MIN_VER = { 0, 26, 1 }

local function check_treesitter()
    local executable_name = 'tree-sitter'
    if vim.fn.executable(executable_name) == 1 then
        local out = vim.trim(vim.fn.system({ executable_name, '--version' }))
        local version = vim.version.parse(out)
        return vim.version.ge(version, TREE_SITTER_MIN_VER)
    end
    return false
end

return {
    {
        'nvim-treesitter/nvim-treesitter',
        branch = 'main',
        dependencies = {},
        lazy = false, -- lazy loading not supported
        build = ':TSUpdate',
        enabled = check_treesitter,
        config = function()
            require('nvim-treesitter').install({
                'awk',
                'bash',
                'c',
                'commonlisp',
                'cpp',
                'diff',
                'elixir',
                'erlang',
                'git_config',
                'git_rebase',
                'gitattributes',
                'gitcommit',
                'gitignore',
                'go',
                'gomod',
                'gosum',
                'gowork',
                'haskell',
                'java',
                'javascript',
                'jq',
                'json',
                'kotlin',
                'lua',
                'markdown_inline',
                'markdown',
                'python',
                'rust',
                'scala',
                'sql',
                'ssh_config',
                'starlark',
                'swift',
                'terraform',
                'tmux',
                'toml',
                'typescript',
                'vim',
                'xml',
                'yaml',
                'zsh',
            })
        end,
    },
    {
        'nvim-treesitter/nvim-treesitter-textobjects',
        branch = 'main',
        lazy = true,
        dependencies = {
            'nvim-treesitter/nvim-treesitter',
        },
        event = { 'BufReadPre', 'BufNewFile' },
        opts = {
            move = {
                set_jumps = true,
            },
        },
        keys = {
            -- Select keymaps
            {
                'ac',
                function()
                    require('nvim-treesitter-textobjects.select').select_textobject('@class.outer')
                end,
                desc = 'Class',
                mode = { 'x', 'o' },
            },
            {
                'af',
                function()
                    require('nvim-treesitter-textobjects.select').select_textobject(
                        '@function.outer'
                    )
                end,
                desc = 'Function',
                mode = { 'x', 'o' },
            },
            {
                'C',
                function()
                    require('nvim-treesitter-textobjects.select').select_textobject(
                        '@conditional.inner'
                    )
                end,
                desc = 'Conditional',
                mode = { 'x', 'o' },
            },
            {
                'ic',
                function()
                    require('nvim-treesitter-textobjects.select').select_textobject('@class.inner')
                end,
                desc = 'Class',
                mode = { 'x', 'o' },
            },
            {
                'if',
                function()
                    require('nvim-treesitter-textobjects.select').select_textobject(
                        '@function.inner'
                    )
                end,
                desc = 'Function',
                mode = { 'x', 'o' },
            },
            -- Swap keymaps
            {
                '<leader>c>',
                function()
                    require('nvim-treesitter-textobjects.swap').swap_next('@parameter.inner')
                end,
                desc = 'Swap next',
            },
            {
                '<leader>c<',
                function()
                    require('nvim-treesitter-textobjects.swap').swap_previous('@parameter.inner')
                end,
                desc = 'Swap previous',
            },
            -- Move keymaps
            {
                '[z',
                function()
                    require('nvim-treesitter-textobjects.move').goto_prev_start('@fold')
                end,
                desc = 'Fold',
                mode = { 'x', 'o' },
            },
            {
                ']z',
                function()
                    require('nvim-treesitter-textobjects.move').goto_next_start('@fold')
                end,
                desc = 'Fold',
                mode = { 'x', 'o' },
            },
            {
                '[c',
                function()
                    require('nvim-treesitter-textobjects.move').goto_prev_start('@class')
                end,
                desc = 'Class',
                mode = { 'x', 'o' },
            },
            {
                ']c',
                function()
                    require('nvim-treesitter-textobjects.move').goto_next_start('@class')
                end,
                desc = 'Class',
                mode = { 'x', 'o' },
            },
            {
                '[C',
                function()
                    require('nvim-treesitter-textobjects.move').goto_prev_start('@conditional')
                end,
                desc = 'Conditional',
            },
            {
                ']C',
                function()
                    require('nvim-treesitter-textobjects.move').goto_next_start('@conditional')
                end,
                desc = 'Conditional',
            },
        },
    },
    {
        'nvim-treesitter/nvim-treesitter-context',
        lazy = true,
        event = { 'VeryLazy' },
        opts = {
            enable = true,
            max_lines = 0, -- unlimited lines
            min_window_height = 0,
            line_numbers = true,
            multiline_threshold = 20,
            trim_scope = 'outer',
            mode = 'topline', -- Options of 'cursor' for current line or 'topline'
            separator = nil,
            zindex = 20,
            on_attach = nil,
        },
        keys = {
            {
                '<leader>cTce',
                vim.cmd.TSContextEnable,
                desc = 'Enable context',
            },
            {
                '<leader>cTcd',
                vim.cmd.TSContextDisable,
                desc = 'Disable context',
            },
            {
                '<leader>cTct',
                function()
                    require('treesitter-context').setup({ mode = 'topline' })
                end,
                desc = 'Topline context',
            },
            {
                '<leader>cTcc',
                function()
                    require('treesitter-context').setup({ mode = 'cursor' })
                end,
                desc = 'Cursor context',
            },
        },
    },
}
