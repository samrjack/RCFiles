DescribeKeys({
    { '<leader>cL', group = 'LSP settings' },
    { 'gr', group = 'LSP' },
})

return {
    {
        'williamboman/mason.nvim',
        version = 'v2.*',
        build = ':MasonUpdate',
        lazy = true,
        opts = {},
        cmd = 'Mason',
        keys = {
            { '<leader>cLM', ':Mason<cr>', desc = 'LSP server manager' },
        },
    },
    {
        'williamboman/mason-lspconfig.nvim',
        version = 'v2.*',
        dependencies = {
            { 'j-hui/fidget.nvim', opts = {}, lazy = true }, -- Nice notifications
            'williamboman/mason.nvim',
            'neovim/nvim-lspconfig',
        },
        lazy = true,
        event = { 'BufReadPre', 'FileType' },
        opts = {
            ensure_installed = {
                -- See here for full list: https://github.com/williamboman/mason-lspconfig.nvim
                -- Maybe replace this section using the mason-tool-installer.nvim package
                'elixirls',
                'gopls',
                'helm_ls',
                'lua_ls',
                'graphql',
                'starpls',
                'ts_ls',
                'jsonls',
                'yamlls',
            },
        },
    },
    {
        'neovim/nvim-lspconfig',
        dependencies = {
            'hrsh7th/cmp-nvim-lsp',
            { 'b0o/SchemaStore.nvim', lazy = true, version = false }, -- For JSON and YAML
        },
        lazy = true,
        config = function()
            -- Adds extra capabilities to LSP list for use with cmp package
            local cmp_lsp_capabilities = require('cmp_nvim_lsp').default_capabilities()
            vim.lsp.config('*', {
                capabilities = cmp_lsp_capabilities,
            })

            vim.lsp.config('helm_ls', {
                settings = {
                    ['helm_ls'] = {
                        filetypes = { 'helm' },
                        yamlls = vim.fn.stdpath('data') .. '/mason/bin/yaml-language-server',
                    },
                },
            })

            vim.lsp.config('lua_ls', {
                -- Mostly pulled from source, only modification is for making vim global work at the bottom.
                -- https://github.com/neovim/nvim-lspconfig/blob/master/doc/configs.md#lua_ls
                on_init = function(client)
                    if client.workspace_folders then
                        local path = client.workspace_folders[1].name
                        if
                            path ~= vim.fn.stdpath('config')
                            and (
                                vim.uv.fs_stat(path .. '/.luarc.json')
                                or vim.uv.fs_stat(path .. '/.luarc.jsonc')
                            )
                        then
                            return
                        end
                    end

                    client.config.settings.Lua =
                        vim.tbl_deep_extend('force', client.config.settings.Lua, {
                            runtime = {
                                version = 'LuaJIT',
                                path = {
                                    'lua/?.lua',
                                    'lua/?/init.lua',
                                },
                            },
                            workspace = {
                                checkThirdParty = false,
                                library = {
                                    vim.api.nvim_get_runtime_file('', true),
                                },
                            },
                        })
                end,
            })

            vim.lsp.config('jsonls', {
                settings = {
                    json = {
                        format = {
                            enable = true,
                        },
                        validate = { enable = true },
                    },
                },
                on_new_config = function(new_config)
                    new_config.settings.json.schemas = new_config.settings.json.schemas or {}
                    vim.list_extend(
                        new_config.settings.json.schemas,
                        require('schemastore').json.schemas()
                    )
                end,
            })

            vim.lsp.config('yamlls', {
                settings = {
                    yaml = {
                        format = {
                            enable = true,
                        },
                        validate = { enable = true },
                        schemas = require('schemastore').yaml.schemas(),
                    },
                },
                on_new_config = function(new_config)
                    new_config.settings.json.schemas = new_config.settings.json.schemas or {}
                    vim.list_extend(
                        new_config.settings.json.schemas,
                        require('schemastore').json.schemas()
                    )
                end,
            })
        end,
    },
}
