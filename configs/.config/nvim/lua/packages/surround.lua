return {
	{
		"kylechui/nvim-surround",
		lazy = true,
		opts = {},
		keys = {
			{ 'ys', desc = 'Add surrounding' },
			{ 'cs', desc = 'Change surrounding' },
			{ 'ds', desc = 'Delete surrounding' },
		}
	},
	{
		'windwp/nvim-autopairs',
		lazy = true,
		event = 'InsertEnter',
		opts = {},
	}
}
