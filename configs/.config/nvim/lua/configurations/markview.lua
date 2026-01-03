local function markdown_functions(buf)
	vim.keymap.set('n', '<leader>cmT', function() vim.cmd.Markview('toggleAll') end,
		{ buffer = buf, desc = 'Toggle global markview' })
	vim.keymap.set('n', '<leader>cmE', function() vim.cmd.Markview('enableAll') end,
		{ buffer = buf, desc = 'Enable global markview' })
	vim.keymap.set('n', '<leader>cmD', function() vim.cmd.Markview('disableAll') end,
		{ buffer = buf, desc = 'Disable global markview' })
	vim.keymap.set('n', '<leader>cmt', function() vim.cmd.Markview('toggle') end,
		{ buffer = buf, desc = 'Toggle markview' })
	vim.keymap.set('n', '<leader>cme', function() vim.cmd.Markview('enable') end,
		{ buffer = buf, desc = 'Enable markview' })
	vim.keymap.set('n', '<leader>cmd', function() vim.cmd.Markview('disable') end,
		{ buffer = buf, desc = 'Disable markview' })
	vim.keymap.set('n', '<leader>cms', function() vim.cmd.Markview('splitToggle') end,
		{ buffer = buf, desc = 'Toggle split view' })
	vim.keymap.set('n', '<leader>cmh', function() vim.cmd.Markview('hybridToggle') end,
		{ buffer = buf, desc = 'Toggle hybrid mode' })
end

local markdown_au_group = vim.api.nvim_create_augroup('markview_trigger_group', {})
vim.api.nvim_create_autocmd('FileType', {
	group = markdown_au_group,
	pattern = 'markdown',
	callback = function(ev)
		markdown_functions(ev.buf)
	end,
})
