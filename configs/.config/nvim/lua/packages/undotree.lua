return {
	'mbbill/undotree',
	lazy = true,
	config = function()
		vim.g.undotree_UndoDir = vim.opt.undodir:get()
		vim.g.undotree_undotree_WindowLayout = 2 -- style 2 has amuch bigger section for seeing the diff
		vim.g.undotree_ShortIndicators = 1 -- Abbreviate times like 'day' into 'd'
		vim.g.undotree_SetFocusWhenToggle = 1
	end,
	keys = {
		{ '<leader>u', vim.cmd.UndotreeToggle, desc = 'undo tree' },
	},
}
