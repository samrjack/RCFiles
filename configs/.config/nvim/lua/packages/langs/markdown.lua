DescribeKeys({
    '<leader>cm',
    cond = function()
        return vim.bo.filetype == 'markdown'
    end,
    group = 'Markdown settings',
})

return {
    {
        'OXY2DEV/markview.nvim',
        lazy = true,
        ft = 'markdown',
        dependencies = {
            'nvim-tree/nvim-web-devicons',
        },
        opts = {},
    },
}
