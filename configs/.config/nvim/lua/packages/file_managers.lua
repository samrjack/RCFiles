return {
    {
        'nvim-neo-tree/neo-tree.nvim',
        branch = 'v3.x',
        dependencies = {
            'nvim-lua/plenary.nvim',
            'nvim-tree/nvim-web-devicons',
            'munifTanjim/nui.nvim',
        },
        lazy = true,
        opts = {
            add_blank_line_at_top = true,
            close_if_last_window = true,
            source_selector = {
                winbar = true, -- Have clickable tabs for showing which mode neotree is in
            },
            window = {
                mappings = {
                    ['^'] = { 'navigate_up' },
                    ['P'] = {
                        'toggle_preview',
                        config = { use_float = true, use_image_nvim = false },
                    },
                    ['<Tab>'] = {
                        function(state)
                            local node = state.tree:get_node()
                            local commands = require('neo-tree.sources.filesystem.commands')
                            local renderer = require('neo-tree.ui.renderer')

                            if node.type == 'directory' then
                                if not node:is_expanded() then
                                    -- Expanding - check if we should return to previous file
                                    commands.toggle_node(state)
                                    if vim.w.neo_tree_last_file and vim.w.neo_tree_last_parent == node:get_id() then
                                        renderer.focus_node(state, vim.w.neo_tree_last_file)
                                        vim.w.neo_tree_last_file = nil
                                        vim.w.neo_tree_last_parent = nil
                                    end
                                else
                                    commands.toggle_node(state)
                                end
                            else
                                -- On a file - store it before collapsing parent
                                local parent_id = node:get_parent_id()
                                if parent_id then
                                    vim.w.neo_tree_last_file = node:get_id()
                                    vim.w.neo_tree_last_parent = parent_id
                                    renderer.focus_node(state, parent_id)
                                    commands.toggle_node(state)
                                end
                            end
                        end,
                        desc = 'Toggle dir',
                    },
                },
                filtered_items = {
                    hide_dotfiles = false,
                    hide_gitignored = false,
                },
            },
            filesystem = {
                window = {
                    mappings = {
                        ['/'] = '',
                        ['F'] = 'fuzzy_finder',
                    },
                },
            },
        },
        keys = {
            { '<C-n>', ':Neotree left reveal toggle<cr>', desc = 'File tree' },
            { 'g.', ':Neotree float reveal toggle<cr>', desc = 'File tree' },
        },
    },
    {
        'stevearc/oil.nvim',
        dependencies = {
            'nvim-tree/nvim-web-devicons',
        },
        lazy = true,
        opts = {
            default_file_explorer = false,
            columns = {
                'icon',
                'permissions',
                'size',
                'mtime',
            },
            view_options = {
                show_hidden = true,
                sort = {
                    { 'name', 'asc' },
                },
            },
            -- Automatically do git operations on the files
            git = {
                add = function()
                    return true
                end,
                mv = function()
                    return true
                end,
                rm = function()
                    return true
                end,
            },
        },
        cmd = { 'Oil' },
        keys = {
            { 'go', vim.cmd.Oil, desc = 'Oil' },
        },
    },
}
