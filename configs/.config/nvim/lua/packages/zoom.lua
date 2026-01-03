return {
	'nyngwang/NeoZoom.lua',
	lazy = true,
	opts = {
		popup = { enabled = false },
		winopts = {
			offset = {
				height = .92,
			},
			border = 'double',
		},
	},
	keys = {
		{ '<C-w>z',    vim.cmd.NeoZoomToggle, desc = 'Zoom' },
		{ '<leader>z', vim.cmd.NeoZoomToggle, desc = 'Zoom' },
	}
}
