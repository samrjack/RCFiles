-- All lsp bindings for working in most code bases

local function show_lsp()
    local active_lsps = vim.lsp.get_clients()
    for _, active_lsp in pairs(active_lsps) do
        print(active_lsp.name)
    end
end

local function setup_lsp_keymaps(buf)
    vim.keymap.set(
        'n',
        '<leader>ca',
        vim.lsp.buf.code_action,
        { buffer = buf, desc = 'LSP Code Action' }
    )
    vim.keymap.set('n', '<leader>cf', vim.lsp.buf.format, { buffer = buf, desc = 'Format code' })
    vim.keymap.set('n', '=', vim.lsp.buf.format, { buffer = buf, desc = 'Format code' })
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { buffer = buf, desc = 'Goto definition' })
    vim.keymap.set(
        'n',
        '<leader>cd',
        vim.lsp.buf.definition,
        { buffer = buf, desc = 'Goto definition' }
    )
    vim.keymap.set(
        'n',
        '<leader>cD',
        vim.lsp.buf.references,
        { buffer = buf, desc = 'Goto references' }
    )
    vim.keymap.set(
        'n',
        '<leader>c?',
        vim.diagnostic.open_float,
        { buffer = buf, desc = 'Diagnostics' }
    )
    vim.keymap.set(
        'n',
        '<leader>ci',
        vim.lsp.buf.implementation,
        { buffer = buf, desc = 'Goto implementation' }
    )
    vim.keymap.set('n', '<leader>cr', vim.lsp.buf.rename, { buffer = buf, desc = 'Rename' })
    vim.keymap.set(
        'n',
        '<leader>ct',
        vim.lsp.buf.type_definition,
        { buffer = buf, desc = 'Goto type definition' }
    )
    vim.keymap.set('n', '<leader>ch', vim.lsp.buf.hover, { buffer = buf, desc = 'Hover' })
    vim.keymap.set(
        'n',
        '<leader>cs',
        vim.lsp.buf.signature_help,
        { buffer = buf, desc = 'Signature help' }
    )
    vim.keymap.set('n', '<leader>cL?', show_lsp, { buffer = buf, desc = 'Signature help' })
    vim.keymap.set('n', '<leader>cLr', ':LspRestart<CR>', { buffer = buf, desc = 'Restart' })
    vim.keymap.set(
        'i',
        '<A-h>',
        vim.lsp.buf.signature_help,
        { buffer = buf, desc = 'Signature help' }
    )
    vim.keymap.set(
        'i',
        '<A-k>',
        vim.lsp.buf.signature_help,
        { buffer = buf, desc = 'Signature help' }
    )
    vim.keymap.set('i', '<A-j>', vim.lsp.buf.hover, { buffer = buf, desc = 'Hover' })

    -- formatting on save
    vim.keymap.set('n', '<leader>cLf', function()
        vim.b[buf].lsp_format_on_save = not vim.b[buf].lsp_format_on_save
    end, { buffer = buf, desc = 'Toggle format on save' })
    vim.keymap.set('n', '<leader>cF', function()
        vim.b[buf].lsp_format_on_save = not vim.b[buf].lsp_format_on_save
    end, { buffer = buf, desc = 'Toggle format on save' })
end

local function inlay_hints(buf, lsp_client)
    if lsp_client ~= {} and lsp_client.server_capabilities.inlayHintProvider then
        -- vim.lsp.inlay_hint.enable(true, { bufnr = buf })
        vim.keymap.set('n', '<leader>cLi', function()
            vim.lsp.inlay_hint.enable(
                not vim.lsp.inlay_hint.is_enabled({ bufnr = buf }),
                { bufnr = buf }
            )
            local status
            if vim.lsp.inlay_hint.is_enabled({ bufnr = buf }) then
                status = 'enabled'
            else
                status = 'disabled'
            end
            vim.notify('inlay hints ' .. status)
        end, { buffer = buf, desc = 'Toggle inlay hints' })
    end
end

local lsp_augroup = vim.api.nvim_create_augroup('lsp_commands', {})

vim.api.nvim_create_autocmd('LspAttach', {
    group = lsp_augroup,
    desc = 'On lsp attach',
    callback = function(event)
        local buf = event.buf
        vim.b[buf].is_in_lsp = true
        vim.b[buf].lsp_format_on_save = true

        local id = vim.tbl_get(event, 'data', 'client_id')
        local lsp_client = {}
        if id then
            lsp_client = vim.lsp.get_client_by_id(id) or {}
        end

        setup_lsp_keymaps(buf)
        inlay_hints(buf, lsp_client)
    end,
})

vim.api.nvim_create_autocmd('BufWritePre', {
    group = lsp_augroup,
    desc = 'Format files with LSP (when enabled)',
    callback = function(event)
        local buf = event.buf
        if vim.b[buf].is_in_lsp and vim.b[buf].lsp_format_on_save then
            vim.lsp.buf.format({ bufnr = buf })
        end
    end,
})

vim.keymap.set('n', '<leader>cLI', vim.cmd.LspInfo, { desc = 'LSP Info' })
