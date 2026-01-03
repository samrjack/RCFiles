return {
	"folke/which-key.nvim",
	lazy = true,
	event = 'VeryLazy',
	opts = {
		preset = "modern",
		sort = { "alphanum" },
		win = {
			height = { min = 6, max = 35 }
		},
		-- Only put which-key specific bindings in here
		spec = {
			{
				'<leader>b',
				group = 'Buffers',
				expand = function()
					return require("which-key.extras").expand.buf()
				end
			},
		},
	},
	config = function(_, opts)
		require('which-key').setup(opts)
		require('keys.which-key-setup').presets()
		require('keys.which-key-setup').defined()
	end,
	keys = {
		{
			'<leader>?',
			function()
				require('which-key').show({ global = false })
			end,
			mode = { 'n', 'x' },
			desc = 'Local keymaps',
		},
		{
			'<leader>/',
			function()
				require('which-key').show({ global = true })
			end,
			mode = { 'n', 'x' },
			desc = 'Global keymaps',
		},
	},
}
