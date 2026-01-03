return {
    {
        'rafamadriz/friendly-snippets',
        lazy = true,
        config = function()
            require('luasnip.loaders.from_vscode').lazy_load()
        end,
    },
    {
        -- This one doesn't quite work yet
        'molleweide/LuaSnip-snippets.nvim',
        lazy = true,
        config = function()
            require('luasnip.loaders.from_lua').lazy_load()
        end,
    },
    {
        'L3MON4D3/LuaSnip',
        dependencies = {
            'rafamadriz/friendly-snippets',
            'molleweide/LuaSnip-snippets.nvim',
        },
        -- follow latest release.
        version = 'v2.*', -- Replace <CurrentMajor> by the latest released major (first number of latest release)
        -- install jsregexp (optional!).
        build = 'make install_jsregexp',
        lazy = true
    },
    {
        'hrsh7th/nvim-cmp',
        dependencies = {
            'L3MON4D3/LuaSnip',
            'saadparwaiz1/cmp_luasnip',
            'zbirenbaum/copilot-cmp', -- AI completion
            'hrsh7th/cmp-nvim-lsp',   -- Complete from lsp suggestions
            'hrsh7th/cmp-nvim-lua',   -- Complete nvim lua api
            'hrsh7th/cmp-buffer',     -- Complete words and items from the buffer
            'hrsh7th/cmp-path',       -- Complete file name
            'hrsh7th/cmp-cmdline',    -- Completion for Vim's commandline
            'onsails/lspkind.nvim',   -- Clean up recommendation box and show source
            'f3fora/cmp-spell',       -- Spelling recommendations
        },
        lazy = true,
        opts = function() -- When opts is a function, it needs to return the table to be used for setup
            local cmp = require('cmp')
            local cmp_select = { behavior = cmp.SelectBehavior.Select }
            return {
                snippet = {
                    expand = function(args)
                        require('luasnip').lsp_expand(args.body)
                    end
                },
                window = {
                    completion = cmp.config.window.bordered(),
                    documentation = cmp.config.window.bordered(),
                },
                mapping = cmp.mapping.preset.insert({
                    ['<C-p>'] = cmp.mapping.select_prev_item(cmp_select),
                    ['<C-n>'] = cmp.mapping.select_next_item(cmp_select),
                    ['<C-y>'] = cmp.mapping({
                        i = function(fallback)
                            if cmp.visible() and cmp.get_active_entry() then
                                cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false })
                            else
                                fallback()
                            end
                        end,
                        c = cmp.mapping.confirm({ select = true })
                    }),
                    ['<CR>'] = cmp.mapping({
                        -- i = function(fallback)
                        -- 	if cmp.visible() and cmp.get_active_entry() then
                        -- 		cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false })
                        -- 	else
                        -- 		fallback()
                        -- 	end
                        -- end,
                        i = cmp.mapping.confirm({ select = true }),
                        s = cmp.mapping.confirm({ select = true }),
                        c = function(fallback)
                            if cmp.visible() and cmp.get_active_entry() then
                                cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false })
                            else
                                fallback()
                            end
                        end,
                    }),
                    ['<C-Space>'] = cmp.mapping.complete(),
                }),
                sources = cmp.config.sources({
                    { name = 'copilot' },
                    { name = 'nvim_lsp' },
                    { name = 'nvim_lua' },
                    { name = 'luasnip' },
                    { name = 'buffer' },
                }, {
                    { name = 'path' },
                }, {
                    { name = 'spell' },
                }),
                formatting = {
                    format = require('lspkind').cmp_format({
                        with_text = true,
                        show_labelDetails = true,
                        menu = {
                            copilot = '[cop]',
                            buffer = '[buf]',
                            nvim_lsp = '[LSP]',
                            nvim_lua = '[api]',
                            path = '[path]',
                            luasnip = '[snip]',
                            spell = '[spell]',
                        }
                    })
                },
                experimental = {
                    ghost_text = true, -- show recommendation as typing takes place
                },
            }
        end,
        config = function(_, opts)
            local cmp = require('cmp')

            cmp.setup(opts)

            cmp.setup.cmdline({ '/', '?' }, {
                mapping = cmp.mapping.preset.cmdline()
            })
            cmp.setup.cmdline({ ':', {
                mapping = cmp.mapping.preset.cmdline(),
                sources = cmp.config.sources({
                    { name = 'path' },
                    { name = 'cmdline' },
                })
            } })
        end,
    },
    {
        'williamboman/mason.nvim',
        version = "v2.*",
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
        version = "v2.*",
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
                            and (vim.uv.fs_stat(path .. '/.luarc.json') or vim.uv.fs_stat(path .. '/.luarc.jsonc'))
                        then
                            return
                        end
                    end

                    client.config.settings.Lua = vim.tbl_deep_extend('force', client.config.settings.Lua, {
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
                    vim.list_extend(new_config.settings.json.schemas, require('schemastore').json.schemas())
                end,
            })

            vim.lsp.config('yamlls', {
                settings = {
                    yaml = {
                        format = {
                            enable = true,
                        },
                        validate = { enable = true },
                        schemas = require('schemastore').yaml.schemas()
                    },
                },
                on_new_config = function(new_config)
                    new_config.settings.json.schemas = new_config.settings.json.schemas or {}
                    vim.list_extend(new_config.settings.json.schemas, require('schemastore').json.schemas())
                end,
            })
        end,
    },
}
