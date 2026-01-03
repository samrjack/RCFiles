return {
	{
		'hat0uma/csvview.nvim',
		lazy = true,
		opts = {
			view = {
				display_mode = "border",
			},
		},
		config = function(_, opts)
			require('csvview').setup(opts)
			require('configurations.csvview')
		end,
		ft = {
			'csv',
		},
		-- keys set up in configurations file
	},
	{
		'cameron-wags/rainbow_csv.nvim',
		lazy = true,
		opt = {},
		ft = {
			'csv',
			'tsv',
			'csv_semicolon',
			'csv_whitespace',
			'csv_pipe',
			'rfc_csv',
			'rfc_semicolon',
		},
		cmd = {
			'RainbowDelim',
			'RainbowDelimSimple',
			'RainbowDelimQuoted',
			'RainbowMultiDelim'
		},
	},
}
