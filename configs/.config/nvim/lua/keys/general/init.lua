-- General keybindings for vim

vim.g.mapleader = ' '
vim.g.maplocalleader = '\\'

require('keys.general.copy_paste')
require('keys.general.easy-escape')
require('keys.general.insert-movements')
require('keys.general.jumping')
require('keys.general.search_replace')
require('keys.general.visual_mode')
