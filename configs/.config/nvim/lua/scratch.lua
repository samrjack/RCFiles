-- This marks any buffer opened from the command line
-- without a file associated as a scratch buffer.
-- This way, the file can be closed without quiestion.

-- Define functions for scratch buffer

local function TurnOnScratchBuffer()
	-- Mark this buffer as a scratch buffer for future checking
	vim.b.isScratchBuffer = true

	vim.opt_local.buftype = 'nofile'
	vim.opt_local.bufhidden = 'hide'
	vim.opt_local.swapfile = false
end

local function TurnOffScratchBuffer()
		vim.b.isScratchBuffer = false

		vim.opt_local.buftype = nil
		vim.opt_local.bufhidden = nil
		vim.opt_local.swapfile = true
end

function NewScratchBuffer()
	vim.cmd.new()
	TurnOnScratchBuffer()
end

vim.api.nvim_create_user_command('NewScratchBuffer', NewScratchBuffer, {})

-- Automate the scratch buffer in certain cases

local id = vim.api.nvim_create_augroup('remove_quite_prompt', {})

-- When input comes from stdin
vim.api.nvim_create_autocmd('StdinReadPre', {
	group = id,
	desc = 'Make data read from stdin into scratch buffer',
	callback = TurnOnScratchBuffer,
})

-- When a file is being used that has no filename
vim.api.nvim_create_autocmd('VimEnter', {
	group = id,
	desc = 'Make new empty buffers treated as scratch buffers',
	callback = function(ev)
		if ev.file == '' and (vim.opt_local.buftype:get() == nil or vim.opt_local.buftype:get() == '') then
			TurnOnScratchBuffer()
		end
	end,
})

-- After saving a file, turn off the scratch buffer attributes
vim.api.nvim_create_autocmd('BufWritePost', {
	group = id,
	desc = 'Turn off scratch buffer status of newly saved files',
	callback = function(ev)
		if vim.b.isScratchBuffer then
			TurnOffScratchBuffer()
			vim.opt_local.modified = false
			vim.cmd.file(ev.file)
			vim.cmd.edit(ev.file)
		end
	end,
})
