-- Lazy
--
-- Configures the lazy package manager so that packages get imported for the correct spots.

-- Bootstrap lazy.nvim so we can guarentee that it's installed
local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = 'https://github.com/folke/lazy.nvim.git'
    local out = vim.fn.system({
        'git',
        'clone',
        '--filter=blob:none',
        '--branch=stable',
        lazyrepo,
        lazypath,
    })

    -- Error handling for when the download fails
    if vim.v.shell_error ~= 0 then
        vim.api.nvim_echo({
            { 'Failed to clone lazy.nvim:\n', 'ErrorMsg' },
            { out, 'WarningMsg' },
            { '\nPress any key to exit...' },
        }, true, {})
        vim.fn.getchar()
        os.exit(1)
    end
end
vim.opt.rtp:prepend(lazypath)

-- Trigger lazy to download all packages defined in the packages module
require('lazy').setup({
    spec = {
        { import = 'packages' },
        { import = 'packages.langs' },
    },
    install = {
        colorscheme = { 'everforest', 'dracula', 'retrobox', 'desert' },
    },
    dev = {
        path = '~/projects/neovim',
        fallback = true, -- If I commit a change with a dev path, it will still use the git repo elsewhere.
    },
    change_detection = { enabled = false, notify = false },
})

vim.keymap.set('n', '<leader>hL', ':Lazy<cr>', { desc = 'Lazy Manager' })
