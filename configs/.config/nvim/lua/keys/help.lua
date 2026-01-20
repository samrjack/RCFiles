vim.api.nvim_create_autocmd('FileType', {
    pattern = 'help',
    callback = function(args)
        vim.keymap.set('n', '<CR>', '<C-]>', { buffer = args.buf, desc = 'Jump to tag' })
        vim.keymap.set('n', 'g==', 'g==', { buffer = args.buf, desc = 'Execute code block' })
    end,
})

DescribeKeys({
    { '<leader>h', group = 'Help' },
    { '<leader>hi', group = 'Info' },
}, { 'n', 'v' })
