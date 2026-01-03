local function toggleHLChunk()
	if vim.g.hlchunkDisabled then
		-- vim.cmd.EnableHLBlank()
		vim.cmd.EnableHLIndent()
		vim.cmd.EnableHLChunk()
		vim.cmd.EnableHLLineNum()
	else
		-- vim.cmd.DisableHLBlank()
		vim.cmd.DisableHLIndent()
		vim.cmd.DisableHLChunk()
		vim.cmd.DisableHLLineNum()
	end
	vim.g.hlchunkDisabled = not vim.g.hlchunkDisabled
end

return {
	{
		"shellRaining/hlchunk.nvim",
		lazy = true,
		opts = {
			chunk = {
				enable = true,
			},
			indent = {
				enable = true,
				use_treesitter = true,
				chars = {
					"│",
				},
				filter_list = {
					function(v)
						return v.level ~= 1
					end,
				},
			},
			blank = {
				enable = false,
			},
			line_num = {
				enable = true,
			},
		},
		config = function(_, opts)
			require('hlchunk').setup(opts)
			toggleHLChunk()
		end,
		keys = {
			{
				'<leader>ti', function() toggleHLChunk() end, desc = 'Toggle Indentation'
			}
		}
	},
	-- Doesn't seem to work as desired
	-- {
	-- 	"lukas-reineke/indent-blankline.nvim",
	-- 	main = "ibl",
	-- 	opts = {
	-- 		enabled = true,
	-- 	},
	-- 	lazy = true,
	-- 	keys = {
	-- 		{
	-- 			'<leader>ti',
	-- 			function()
	-- 				require("ibl").setup_buffer(0, {
	-- 					enabled = not require("ibl.config").get_config(0).enabled,
	-- 				})
	-- 			end,
	-- 			desc = 'Toggle indentation'
	-- 		},
	-- 	}
	-- },
}
