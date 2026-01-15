-- For use by which key; do not import elsewhere. It probably wouldn't cause an issue, but it's simply not necessary.

local M = {}

M.defined_n_x_operators = {
    mode = { 'n', 'x' },
    { '<leader>', group = 'Leader' },
    { '<leader>w', group = 'Windows', proxy = '<C-w>' },
}

M.defined_n_v_operators = {
    mode = { 'n', 'v' },
    { '<leader><Enter>', group = 'Bookmarks' },
    { '<leader>a', group = 'AI' },
    { '<leader>c', group = 'Code' },
    { '<leader>cL', group = 'LSP settings' },
    { '<leader>cT', group = 'Treesitter' },
    { '<leader>cTc', group = 'Treesitter context' },
    { '<leader>cm', group = 'Markdown settings' },
    { '<leader>f', group = 'File' },
    { '<leader>g', group = 'Git' },
    { '<leader>gh', group = 'Hunks' },
    { '<leader>h', group = 'Help' },
    { '<leader>s', group = 'Search/Replace' },
    { '<leader>t', group = 'Toggle' },
    { '<leader>x', group = 'Diagnostics' },
    { 'gp', group = 'Preview' },
}

function M.defined()
    local wk = require('which-key')
    wk.add(M.defined_n_x_operators)
    wk.add(M.defined_n_v_operators)
end

M.preset_operators = {
    preset = true,
    mode = { 'n', 'x' },
    { 'g', desc = 'Go to' },
}

M.preset_n_operators = {
    preset = true,
    mode = { 'n' },
    { '<C-i>', desc = 'Next Jumplist' },
    { '<C-o>', desc = 'Previous Jumplist' },
    { '[', group = 'Next items' },
    { ']', group = 'Previous items' },
}

M.preset_text_objects = {
    preset = true,
    mode = { 'o', 'x' },
    { '[', group = 'Previous' },
    { ']', group = 'Next' },
}

M.preset_navigation = {
    preset = true,
    mode = { 'n', 'x' },
    { 'H', desc = 'Home line of window (top)' },
    { 'L', desc = 'Last line of window' },
    { 'M', desc = 'Middle line of window' },
    { '[%', desc = 'Previous unmatched group' },
    { '[(', desc = 'Previous (' },
    { '[<', desc = 'Previous <' },
    { '[f', desc = 'Go to file' },
    { '[M', desc = 'Previous method end' },
    { '[m', desc = 'Previous method start' },
    { '[s', desc = 'Previous misspelled word' },
    { '[{', desc = 'Previous {' },
    { ']%', desc = 'Next unmatched group' },
    { '])', desc = 'Next (' },
    { ']>', desc = 'Next <' },
    { ']f', desc = 'Go to file' },
    { ']M', desc = 'Next method end' },
    { ']m', desc = 'Next method start' },
    { ']s', desc = 'Next misspelled word' },
    { ']{', desc = 'Next {' },
}

function M.presets()
    local wk = require('which-key')
    wk.add(M.preset_operators)
    wk.add(M.preset_text_objects)
end

return M
