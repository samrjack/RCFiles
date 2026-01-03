return {
	{
		-- Easily search google for stuff
		'aliqyan-21/wit.nvim',
		lazy = true,
		opts = {},
		keys = {
			{ '<leader>so', ':WitSearch ',           mode = { 'n' }, desc = 'Online' },
			{ '<leader>so', vim.cmd.WitSearchVisual, mode = { 'v' }, desc = 'Online' }
		},
	},
}
