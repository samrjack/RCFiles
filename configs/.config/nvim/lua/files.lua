-- This manages how neovim manages files

-- turn of backups, who needs em'?
vim.opt.swapfile = false
vim.opt.backup = false

-- Undo info
vim.opt.undodir = vim.fn.stdpath('data') .. '/undodir'
vim.opt.undofile = true
vim.opt.undolevels = 10000
