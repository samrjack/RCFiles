return {
    {
        'goolord/alpha-nvim',
        lazy = true,
        event = 'VimEnter',
        dependencies = {
            'nvim-tree/nvim-web-devicons',
            'nvim-lua/plenary.nvim',
        },
        config = function()
            local configuration = require('alpha.themes.theta')
            configuration.file_icons.provider = 'devicons'

            -- Find and override button by shortcut key
            local function find_button(shortcut)
                for _, button in ipairs(configuration.buttons.val) do
                    if button.opts and button.opts.shortcut == shortcut then
                        return button
                    end
                end
            end

            find_button('e').opts.keymap[3] = '<cmd>NewScratchWindow<CR>' -- keymap[3] is the command

            require('alpha').setup(configuration.config)

            -- Set up keymaps when alpha buffer opens
            vim.api.nvim_create_autocmd('FileType', {
                pattern = 'alpha',
                callback = function(event)
                    local opts = { buffer = event.buf, noremap = true, silent = true }

                    -- i opens new scratch buffer in insert mode
                    vim.keymap.set('n', 'i', function()
                        NewScratchWindow()
                        vim.cmd('startinsert')
                    end, opts)

                    -- p/P opens new buffer and pastes
                    local function paste_in_new_buffer()
                        NewScratchWindow()
                        vim.cmd('normal! "+p')
                    end
                    vim.keymap.set('n', 'p', function()
                        NewScratchWindow()
                        vim.cmd('normal! p')
                    end, opts)
                    vim.keymap.set('n', 'P', function()
                        NewScratchWindow()
                        vim.cmd('normal! P')
                    end, opts)
                    vim.keymap.set('n', '<D-v>', paste_in_new_buffer, opts) -- Cmd+V (GUI only)
                    vim.keymap.set('n', '<C-v>', paste_in_new_buffer, opts) -- Ctrl+V
                    vim.keymap.set('n', '<C-r>', function()
                        local register = vim.fn.nr2char(vim.fn.getchar())
                        NewScratchWindow()
                        vim.cmd('normal! "' .. register .. 'p')
                    end, opts) -- Ctrl+R{register}
                end,
            })

            -- Setup paste
            local paste_function = vim.paste

            vim.paste = function(lines, phase)
                local bufnr = vim.api.nvim_get_current_buf()

                -- Check for alpha filetype
                if vim.bo[bufnr].filetype == 'alpha' then
                    NewScratchWindow()
                end

                -- Fall back to default behavior for all other buffers
                return paste_function(lines, phase)
            end
        end,
        keys = {
            { '<leader>hd', '<cmd>Alpha<enter>', desc = 'Dashboard' },
        },
    },
}
