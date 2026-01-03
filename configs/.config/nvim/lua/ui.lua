-- Options that change the basic underlying vim UI settings

vim.opt.number = true        -- Turn on line numbers
vim.opt.smartindent = true   -- Automatically indent new lines
vim.opt.termguicolors = true -- Enable 24-bit RGB colors

-- indentation
vim.opt.tabstop = 4      -- Number of spaces a tab represents
vim.opt.shiftwidth = 4   -- Number of visual spces per tab
vim.opt.softtabstop = 4  -- number of spaces in a tab when editing
vim.opt.expandtab = true -- Use spaces instead of tabs

-- wraping lines when they're too long to view
vim.opt.wrap = false
vim.opt.cursorline = true

-- searching
vim.opt.hlsearch = true
vim.opt.incsearch = true

-- Scrolling with cursor
vim.opt.scrolloff = 8

-- refresh
vim.opt.updatetime = 500 -- defaults to 4000

-- Allow cursor to go into areas without text when in visual block mode
vim.opt.virtualedit = 'block'

-- Highlighted column
vim.g.colorcolumn = { 80, 120 }

-- Highlight copied text
vim.api.nvim_create_autocmd('TextYankPost', {
    group = vim.api.nvim_create_augroup('highlight_yank', {}),
    desc = 'Hightlight selection on yank',
    pattern = '*',
    callback = function()
        vim.highlight.on_yank({ higroup = 'IncSearch', timeout = 400 })
    end,
})

-- Dictionary for spelling corrections
vim.opt.dictionary:append({ '/usr/share/dict/words' })
