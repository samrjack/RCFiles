return {
	{
		'https://github.com/tpope/vim-speeddating',
		lazy = true,
		keys = {
			{ '<C-a>',  mode = { 'n', 'v' }, desc = 'Increment number/date' },
			{ '<C-x>',  mode = { 'n', 'v' }, desc = 'Decrement number/date' },
			{ 'd<C-a>', mode = 'n',          desc = 'Set date UTC' },
			{ 'd<C-x>', mode = 'n',          desc = 'Set date local' },
		},
	},
	{
		'itchyny/calendar.vim',
		lazy = true,
		cmd = "Calendar"
	}
}
