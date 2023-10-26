vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
  -- Автоустановка пакетного менеджера
  use 'wbthomason/packer.nvim'
  ---------------------------------------------------------
  -- ПЛАГИНЫ ВНЕШНЕГО ВИДА
  ---------------------------------------------------------
  -- Информационная строка внизу
  use "kyazdani42/nvim-web-devicons"
  use { 'nvim-lualine/lualine.nvim',
      requires = {'kyazdani42/nvim-web-devicons', opt = true},
      config = function()
      require('lualine').setup()
  end, }
  -- Тема в стиле Rose Pine
use({
    'rose-pine/neovim',
    as = 'rose-pine',
    config = function()
        require("rose-pine").setup()
        vim.cmd('colorscheme rose-pine')
    end
})
  ---------------------------------------------------------
  -- МОДУЛИ РЕДАКТОРА
  ---------------------------------------------------------
  -- Табы с вкладками сверху
  use {'akinsho/bufferline.nvim',
      requires = 'kyazdani42/nvim-web-devicons',
      config = function()
      require("bufferline").setup{}
    end, }
  -- Структура классов и функций в файле
  use 'majutsushi/tagbar'
  -- Файловый менеджер
  use({
	"Pocco81/auto-save.nvim",
	config = function()
		 require("auto-save").setup {
          enabled = true, -- start auto-save when the plugin is loaded (i.e. when your package manager loads it)
    execution_message = {
		message = function() -- message to print on save
			return ("AutoSave: saved at " .. vim.fn.strftime("%H:%M:%S"))
		end,
		dim = 0.18, -- dim the color of `message`
		cleaning_interval = 1250, -- (milliseconds) automatically clean MsgArea after displaying `message`. See :h MsgArea
	},
    trigger_events = {"InsertLeave", "TextChanged"}, -- vim events that trigger auto-save. See :h events
	-- function that determines whether to save the current buffer or not
	-- return true: if buffer is ok to be saved
	-- return false: if it's not ok to be saved
	condition = function(buf)
		local fn = vim.fn
		local utils = require("auto-save.utils.data")

		if
			fn.getbufvar(buf, "&modifiable") == 1 and
			utils.not_in(fn.getbufvar(buf, "&filetype"), {}) then
			return true -- met condition(s), can save
		end
		return false -- can't save
	end,
    write_all_buffers = false, -- write all buffers when the current one meets `condition`
    debounce_delay = 135, -- saves the file at most every `debounce_delay` milliseconds
	callbacks = { -- functions to be executed at different intervals
		enabling = nil, -- ran when enabling auto-save
		disabling = nil, -- ran when disabling auto-save
		before_asserting_save = nil, -- ran before checking `condition`
		before_saving = nil, -- ran before doing the actual save
		after_saving = nil -- ran after doing the actual save
	}
			-- your config goes here
			-- or just leave it empty :)
		 }
	end,
  })

  use { 'kyazdani42/nvim-tree.lua',
      requires = 'kyazdani42/nvim-web-devicons',
      config = function() 
      require'nvim-tree'.setup {}
  end, }
  --- popup окошки
  use 'nvim-lua/popup.nvim'
  ---------------------------------------------------------
  -- ПОИСК
  ---------------------------------------------------------
  -- Наш FuzzySearch
  use { 'nvim-telescope/telescope.nvim',
      requires = { {'nvim-lua/plenary.nvim'} },
      config = function() 
      require'telescope'.setup {}
  end, }
    
  ---------------------------------------------------------
  -- КОД
  ---------------------------------------------------------
  -- автоматические закрывающиеся скобки
  use { 'windwp/nvim-autopairs',
      config = function()
      require("nvim-autopairs").setup()
  end}
  --Комментирует по <gc> все, вне зависимости от языка программирования
 use { 'numToStr/Comment.nvim',
     config = function() 
   require('Comment').setup() 
  end }

  ---------------------------------------------------------
  -- LSP И АВТОДОПОЛНЯЛКИ
  ---------------------------------------------------------
  -- Collection of configurations for built-in LSP client
  use 'neovim/nvim-lspconfig'
  use 'williamboman/nvim-lsp-installer'

  -- Автодополнялка
  use 'hrsh7th/nvim-cmp'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'saadparwaiz1/cmp_luasnip'

  --- Автодополнлялка к файловой системе
  use 'hrsh7th/cmp-path'

  -- Snippets plugin
  use 'L3MON4D3/LuaSnip'

  -- Highlight, edit, and navigate code using a fast incremental parsing library
  use 'nvim-treesitter/nvim-treesitter'

  -- Линтер, работает для всех языков
  use 'dense-analysis/ale'
  ---------------------------------------------------------
  -- РАЗНОЕ
  ---------------------------------------------------------
  -- Даже если включена русская раскладка, то nvim-команды будут работать
  use 'powerman/vim-plugin-ruscmd'
end)  
