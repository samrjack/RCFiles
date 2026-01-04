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
            require('alpha').setup(configuration.config)
        end,
    },
}
