local function neocodeium_status()
    -- Tables to map serverstatus and status to corresponding symbols
    local server_status_symbols = {
        [0] = '󰣺', -- Connected
        [1] = '󱤚', -- Connecting
        [2] = '󰣽', -- Disconnected
    }

    local status_symbols = {
        [0] = '󰚩', -- Enabled
        [1] = '󱚧', -- Disabled Globally
        [3] = '󱚢', -- Disabled for Buffer filetype
        [5] = '󱚠', -- Disabled for Buffer encoding
        [2] = '󱙻', -- Disabled for Buffer (catch-all)
    }

    local function connect(a, b)
        return a .. ' | ' .. b
    end

    if not require('lazy.core.config').plugins['neocodeium']._.loaded then
        return connect('󰣼', status_symbols[2])
    end
    local status, serverstatus = require('neocodeium').get_status()

    -- Handle serverstatus and status fallback (safeguard against any unexpected value)
    local luacodeium = server_status_symbols[serverstatus] or '󰣼'
    luacodeium = connect(luacodeium, (status_symbols[status] or status_symbols[2]))

    return luacodeium
end

local function get_lsp_client()
    local active_lsps = vim.lsp.get_clients()
    for _, active_lsp in pairs(active_lsps) do
        return active_lsp.name
    end
    return ''
end

return {
    {
        'nvim-lualine/lualine.nvim',
        dependencies = { 'nvim-tree/nvim-web-devicons' },
        lazy = true,
        event = 'VimEnter',
        opts = {
            options = {
                globalstatus = true,
            },
            tabline = {
                lualine_a = { 'buffers' },
                lualine_b = { '' },
                lualine_c = { 'filesize', 'filename' },
                lualine_x = { 'searchcount', 'selectioncount' },
                lualine_y = { get_lsp_client, { 'datetime', style = 'iso' } },
                lualine_z = { 'tabs' },
            },
            sections = {
                lualine_a = { 'mode' },
                lualine_b = { 'branch', 'diff', 'diagnostics' },
                lualine_c = { { 'filename', path = 3 } },
                lualine_x = { 'encoding', 'fileformat', 'filetype', neocodeium_status },
                lualine_y = { 'progress' },
                lualine_z = { 'location' },
            },
            inactive_sections = {
                lualine_a = {},
                lualine_b = { 'diff' },
                lualine_c = { { 'filename', path = 3 } },
                lualine_x = { 'location' },
                lualine_y = {},
                lualine_z = {},
            },
            extensions = {
                'fugitive',
                'lazy',
                'nvim-dap-ui',
                'nvim-tree',
                'oil',
            },
        },
    },
    {
        'b0o/incline.nvim',
        dependencies = {
            'nvim-tree/nvim-web-devicons',
            'lewis6991/gitsigns.nvim',
        },
        lazy = true,
        event = 'VimEnter',
        opts = {
            hide = {
                only_win = true,
            },
            render = function(props)
                local filename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(props.buf), ':t')
                if filename == '' then
                    filename = '[No Name]'
                end
                local ft_icon, ft_color = require('nvim-web-devicons').get_icon_color(filename)

                local function get_git_diff()
                    local icons = { removed = '', changed = '', added = '' }
                    local signs = vim.b[props.buf].gitsigns_status_dict
                    local labels = {}
                    if signs == nil then
                        return labels
                    end
                    for name, icon in pairs(icons) do
                        if tonumber(signs[name]) and signs[name] > 0 then
                            table.insert(
                                labels,
                                { icon .. signs[name] .. ' ', group = 'Diff' .. name }
                            )
                        end
                    end
                    if #labels > 0 then
                        table.insert(labels, { '┊ ' })
                    end
                    return labels
                end

                local function get_diagnostic_label()
                    local icons = { error = '', warn = '', info = '', hint = '' }
                    local label = {}

                    for severity, icon in pairs(icons) do
                        local n = #vim.diagnostic.get(
                            props.buf,
                            { severity = vim.diagnostic.severity[string.upper(severity)] }
                        )
                        if n > 0 then
                            table.insert(
                                label,
                                { icon .. n .. ' ', group = 'DiagnosticSign' .. severity }
                            )
                        end
                    end
                    if #label > 0 then
                        table.insert(label, { '┊ ' })
                    end
                    return label
                end

                return {
                    { get_diagnostic_label() },
                    { get_git_diff() },
                    { (ft_icon or '') .. ' ', guifg = ft_color, guibg = 'none' },
                    {
                        filename .. ' ',
                        gui = vim.bo[props.buf].modified and 'bold,italic' or 'bold',
                    },
                    {
                        '┊  ' .. vim.api.nvim_win_get_number(props.win),
                        group = 'DevIconWindows',
                    },
                }
            end,
        },
        keys = {
            {
                '<leader>th',
                function()
                    require('incline').toggle()
                end,
                desc = 'Filename header',
            },
        },
    },
}
