-- These maps are for shortcuts that I use often enough to define as their own key cord.

-- Take the line above the curent line and append it to the end
-- of the current line.
--
-- ex:
-- hello
-- world|
-- |
-- v
-- world| hello
vim.keymap.set('n', '<C-j>', 'kddpkJ', { desc = 'Append previous line' })

vim.keymap.set('n', '<leader>tc', function()
	if next(vim.opt.colorcolumn:get()) == nil then
		vim.opt.colorcolumn = vim.g.colorcolumn
	else
		vim.opt.colorcolumn = {}
	end
end, { desc = 'Toggle color columns' })
