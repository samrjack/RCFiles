return {
	{
		"OXY2DEV/markview.nvim",
		lazy = true,
		ft = "markdown",
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"nvim-tree/nvim-web-devicons"
		},
		config = function()
			require('configurations.markview')
		end
	},
}
