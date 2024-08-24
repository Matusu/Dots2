return {
	"brenoprata10/nvim-highlight-colors",
	config = function()
		vim.opt.termguicolors = true
		require("nvim-highlight-colors").setup({
         virtual_symbol_prefix = '',
         virtual_symbol_position = 'eol',
         render = 'virtual',
			virtual_symbol = "■",
         enable_named_colors = true,
		})
	end,
}
