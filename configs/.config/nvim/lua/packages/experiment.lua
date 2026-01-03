-- This Module is for all the packages I want to experiemnt with in the future
-- but haven't fully configured yet or don't know if I want to keep them.

return {
	{
		'samharju/yeet.nvim',
		dependencies = {
			'stevearc/dressing.nvim', -- optional, provides sane UX
		},
		lazy = true,
		version = '*', -- use the latest release, remove for master
		cmd = 'Yeet',
		opts = {},
	},
	{
		'rmagatti/goto-preview',
		lazy = true,
		opts = {},
		keys = {
			{ 'gpd', function() require('goto-preview').goto_preview_definition() end,      desc = 'Preview definition' },
			{ 'gpt', function() require('goto-preview').goto_preview_type_definition() end, desc = 'Preview type definition' },
			{ 'gpi', function() require('goto-preview').goto_preview_implementation() end,  desc = 'Preview implementation' },
			{ 'gpD', function() require('goto-preview').goto_preview_declaration() end,     desc = 'Preview declarations' },
			{ 'gP',  function() require('goto-preview').close_all_win() end,                desc = 'Close preivews' },
			{ 'gpr', function() require('goto-preview').goto_preview_references() end,      desc = 'Preview references' },
		},
	},
	{
		"NStefan002/screenkey.nvim",
		lazy = true,
		version = "*", -- or branch = "dev", to use the latest commit
		keys = {
			{ '<leader>hk', function() vim.cmd.Screenkey('toggle') end, desc = 'Key logger' }
		},
	}
}
