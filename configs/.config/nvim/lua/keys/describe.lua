local _pending_keys = {}

function DescribeKeys(keys, modes)
    modes = modes or 'n'

    -- Normalize single key to table of keys
    if type(keys[1]) == 'string' then
        keys = { keys }
    end

    local spec = { mode = modes }
    for _, key in ipairs(keys) do
        table.insert(spec, key)
    end

    if package.loaded['which-key'] then
        require('which-key').add(spec)
    else
        table.insert(_pending_keys, spec)
    end
end

-- Only to be run by which-key. Enables any pre-registered keys
function ApplyKeyDescriptions()
    local wk = require('which-key')
    for _, spec in ipairs(_pending_keys) do
        wk.add(spec)
    end
    _pending_keys = {}
end

local generic_groups = {
    { '<leader><Enter>', group = 'Bookmarks' },
    { '<leader>c', group = 'Code' },
    { '<leader>f', group = 'File' },
    { '<leader>s', group = 'Search/Replace' },
    { '<leader>t', group = 'Toggle' },
    { '<leader>w', group = 'Windows', proxy = '<C-w>' },
}

DescribeKeys(generic_groups, { 'n', 'v' })

local preset_operators = {
    { 'g', desc = 'Go to' },
    { '<localleader>', group = 'Local leader' },
    { '<leader>', group = 'Leader' },
}

DescribeKeys(preset_operators, { 'n', 'v' })

local root_mappings_n = {
    { '<C-i>', desc = 'Next Jumplist' },
    { '<C-o>', desc = 'Previous Jumplist' },
    { '[', group = 'Next items' },
    { ']', group = 'Previous items' },
}

DescribeKeys(root_mappings_n, { 'n' })

local navigation = {
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

DescribeKeys(navigation, { 'n', 'v' })

local text_objects = {
    { '[', group = 'Previous' },
    { ']', group = 'Next' },
}

DescribeKeys(text_objects, { 'o', 'v' })
