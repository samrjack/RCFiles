function check_treesitter()
    return vim.fn.executable('tree-sitter') == 1
end

if check_treesitter() then
    return {
        {
            'nvim-treesitter/nvim-treesitter',
            dependencies = {
            },
            lazy = false, -- lazy loading not supported
            build = ':TSUpdate',
            config = function()
                require('nvim-treesitter').install({
                    'bash',
                    'c',
                    'cpp',
                    'diff',
                    'erlang',
                    'git_config',
                    'git_rebase',
                    'gitcommit',
                    'gitignore',
                    'go',
                    'gomod',
                    'gosum',
                    'gowork',
                    'jq',
                    'json',
                    'kotlin',
                    'lua',
                    'markdown',
                    'markdown_inline',
                    'python',
                    'sql',
                    'starlark',
                    'xml',
                    'yaml',
                })
            end,
        },
        {
            'nvim-treesitter/nvim-treesitter-textobjects',
            lazy = true,
            dependencies = {
                'nvim-treesitter/nvim-treesitter',
            },
            event = { 'BufReadPre', 'BufNewFile' },
            -- For future text objects, check https://github.com/nvim-treesitter/nvim-treesitter-textobjects
            opts = {
                textobjects = {
                    select = {
                        enable = true,
                        keymaps = {
                            ['ac'] = { query = '@class.outer', desc = 'Class' },
                            ['af'] = { query = '@function.outer', desc = 'Function' },
                            ['c'] = { query = '@conditional.inner', desc = 'Conditional' },
                            ['ic'] = { query = '@class.inner', desc = 'Class' },
                            ['if'] = { query = '@function.inner', desc = 'Function' },
                        }
                    },
                    swap = {
                        enable = true,
                        swap_next = {
                            ['<leader>c>'] = '@parameter.inner',
                        },
                        swap_previous = {
                            ['<leader>c<'] = '@parameter.inner',
                        },
                    },
                    move = {
                        enable = true,
                        set_jumps = false,
                        goto_next_start = {
                            [']z'] = { query = '@fold', query_group = 'folds', desc = 'Next fold' },
                        },
                        goto_next_end = {
                            [']C'] = '@class.outer'
                        },
                    }
                },
            },
            main = 'nvim-treesitter.configs',
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
                on_atach = nil,
            },
            keys = {
                { '<leader>cTce', vim.cmd.TSContextEnable,                                                  desc = 'Enable context' },
                { '<leader>cTcd', vim.cmd.TSContextDisable,                                                 desc = 'Disable context' },
                { '<leader>cTct', function() require('treesitter-context').setup({ mode = 'topline' }) end, desc = 'Topline context' },
                { '<leader>cTcc', function() require('treesitter-context').setup({ mode = 'cursor' }) end,  desc = 'Cursor context' },
            }
        }
    }
else
    return {}
end
