--vars
local keymap = vim.api.nvim_set_keymap
local default_opts = {noremap = true, silent = true}
local cmd = vim.cmd             -- execute Vim commands
local exec = vim.api.nvim_exec  -- execute Vimscript
local g = vim.g                 -- global variables
local opt = vim.opt             -- global/buffer/windows-scoped options

----------------------------------------------
--Basic Config
----------------------------------------------
cmd("set autochdir") -- follow the files path 
cmd("set noswapfile")
-- identatation
opt.shiftwidth = 4        -- shift 4 spaces when tab
opt.tabstop = 4           -- 1 tab == 4 spaces
opt.smartindent = true    -- autoindent new lines
----------------------------------------------
-- Nice features
----------------------------------------------
-- Запоминает где nvim последний раз редактировал файл
cmd [[
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
]]
---------------------------------------------
-- Appearance
---------------------------------------------
-- Tagbar
g.tagbar_compact = 1
g.tagbar_sort = 0
cmd("set number relativenumber")
-- Always centered verticaly
cmd("set so=999")
-- removes pissing me off gray line on the left
vim.o.signcolumn = 'number' --and display signs in the number column.
-- Подсвечивает на доли секунды скопированную часть текста
exec([[
augroup YankHighlight
autocmd!
autocmd TextYankPost * silent! lua vim.highlight.on_yank{higroup="IncSearch", timeout=700}
augroup end
]], false)
-- Sets colors to line numbers Above, Current and Below  in this order
function LineNumberColors()
    vim.api.nvim_set_hl(0, 'LineNrAbove', { fg='#51B3EC', bold=true })
    vim.api.nvim_set_hl(0, 'LineNr', { fg='white', bold=true })
    vim.api.nvim_set_hl(0, 'LineNrBelow', { fg='#FB508F', bold=true })
end

----------------------------------------
-- Keymaps	
----------------------------------------
cmd("tnoremap <Esc> <C-\\><C-n>") -- escape terminal 
cmd("nnoremap <silent> <esc> :noh<cr><esc>") -- disable search highlight
-- search from home dir
keymap('n', '<C-h>', [[:cd<cr><cmd>lua require('telescope.builtin').find_files()<cr><BS>]], default_opts)
-- search current dir 
keymap('n', '<C-c>', [[<cmd>lua require('telescope.builtin').find_files()<cr><BS>]], default_opts)
-- search buffers
keymap('n', '<C-p>', [[<cmd>lua require('telescope.builtin').buffers()<cr>]], default_opts)

keymap("n",	"<C-cr>",	"o<Esc>0",{ noremap = true })

keymap("n",	"<F4>",		":only <cr>",{ noremap = true })
keymap("n",	"<F3>",		":close <cr>",{ noremap = true })
keymap("n",	"<F2>", 	":tabnew <cr>:terminal<cr>i",{ noremap = true })
keymap("n",	"<F1>", 	":help<cr>:only<cr>",{ noremap = true })

keymap('n',	"<Tab>",   	':BufferLineCycleNext<CR>', default_opts)
keymap('n',	"<S-Tab>", 	':BufferLineCyclePrev<CR>', default_opts)

----------------------------------------------
--Lazy package manager auto install
----------------------------------------------

local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'

-- Auto-install lazy.nvim if not present
if not vim.loop.fs_stat(lazypath) then
  print('Installing lazy.nvim....')
  vim.fn.system({
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', -- latest stable release
    lazypath,
  })
  print('Done.')
end

vim.opt.rtp:prepend(lazypath)

----------------------------------------
-- List of plugins
----------------------------------------
require('lazy').setup({
-- Lsp
  {
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v2.x',
    dependencies = {
      -- LSP Support
      {'neovim/nvim-lspconfig'},             -- Required
      {'williamboman/mason.nvim'},           -- Optional
      {'williamboman/mason-lspconfig.nvim'}, -- Optional

      -- Autocompletion
      {'hrsh7th/nvim-cmp'},     -- Required
      {'hrsh7th/cmp-nvim-lsp'}, -- Required
      {'L3MON4D3/LuaSnip'},     -- Required
    }
  },
-- Other Plugins
  	{'williamboman/mason.nvim'},
	{'williamboman/mason-lspconfig.nvim'},
	{'nvim-telescope/telescope.nvim'},
	{'nvim-lua/plenary.nvim'}, -- Telescope Required
--Rusian vim
  	{'powerman/vim-plugin-ruscmd'},
-- Обрамляет или снимает обрамление. Выдели слово, нажми S и набери <h1>
 	{"kylechui/nvim-surround",
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    config = function()
        require("nvim-surround").setup({
            -- Configuration here, or leave empty to use defaults
        })
    end
	},
-- TreeSitter
 	{'nvim-treesitter/nvim-treesitter'},
-- coloring lsp 
	{'folke/lsp-colors.nvim'},
-- Может повторять через . vimsurround
  	{'tpope/vim-repeat'},
-- Стартовая страница
  	{'mhinz/vim-startify'},
-- Комментирует по gc все, вне зависимости от языка программирования
    {'numToStr/Comment.nvim'},
-- Nice buffers-tabs
	{'akinsho/bufferline.nvim', version = "*", dependencies = 'nvim-tree/nvim-web-devicons'},
-- Autosave
 	{'Pocco81/auto-save.nvim'},
-- Закрывает автоматом скобки
	{'cohama/lexima.vim'},
-- gruvbox
	{ "ellisonleao/gruvbox.nvim", priority = 1000 },
-- unity support
-- 	{"neoclide/coc.nvim", branch = 'release'},
	{"OmniSharp/omnisharp-vim"}
})
	
--Lsp config
local lsp = require('lsp-zero').preset({})

lsp.on_attach(function(client, bufnr)
  -- see :help lsp-zero-keybindings
  -- to learn the available actions
  lsp.default_keymaps({buffer = bufnr})
end)

lsp.setup()
local lspconfig = require('lspconfig')
local lsp_defaults = lspconfig.util.default_config

lsp_defaults.capabilities = vim.tbl_deep_extend(
  'force',
  lsp_defaults.capabilities,
  require('cmp_nvim_lsp').default_capabilities()
)

vim.api.nvim_create_autocmd('LspAttach', {
  desc = 'LSP actions',
  callback = function(event)
    local opts = {buffer = event.buf}

    vim.keymap.set('n', 'K', '<cmd>lua vim.lsp.buf.hover()<cr>', opts)
    vim.keymap.set('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', opts)
    vim.keymap.set('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', opts)
    vim.keymap.set('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<cr>', opts)
    vim.keymap.set('n', 'go', '<cmd>lua vim.lsp.buf.type_definition()<cr>', opts)
    vim.keymap.set('n', 'gr', '<cmd>lua vim.lsp.buf.references()<cr>', opts)
    vim.keymap.set('n', 'gs', '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts)
    vim.keymap.set('n', 'gl', '<cmd>lua vim.diagnostic.open_float()<cr>', opts)
    vim.keymap.set('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<cr>', opts)
    vim.keymap.set('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<cr>', opts)
  end
})
---------------------------------------
-- Autocomplete by tab
local cmp = require('cmp')
cmp.setup{
	mapping = {
	--['<CR>'] = cmp.config.disable,
	['<CR>'] = cmp.mapping.confirm({ select = true }),
	['<Tab>'] = cmp.mapping.confirm({ select = true }),
	}
}

-- call mason.nvim if installed 
require('mason').setup()
require('mason-lspconfig').setup()
require('Comment').setup()

--vim.opt.termguicolors = true -- bufferline requirement
require("bufferline").setup{}
-- Coloring numbers and theme
LineNumberColors()
vim.o.background = "dark" -- or "light" for light mode
require("gruvbox").setup({
  undercurl = false,
  underline = false,
  bold = true,
  italic = {
    strings = true,
    comments = true,
    operators = false,
    folds = true,
  },
  strikethrough = true,
  invert_selection = false,
  invert_signs = false,
  invert_tabline = false,
  invert_intend_guides = false,
  inverse = true, -- invert background for search, diffs, statuslines and errors
  contrast = " ", -- can be "hard", "soft" or empty string
  palette_overrides = {

  dark0_hard = "#000000",
  dark0 = "#282828",
  dark0_soft =  "#000000", --"#32302f",
  dark1 = "#000000", -- "#3c3836",
  dark2 = "#504945", -- Bottom bar with text background
  dark3 = "#665c54", --visual selection color 
  dark4 =  "#7c6f64", -- current line number
  light0_hard = "#000000", --"#f9f5d7", 
  light0 = "#fbf1c7", --suggestions font color
  light0_soft = "#000000", --"#f2e5bc",
  light1 = "#ebdbb2", --the actual font color
  light2 = "#000000", --"#d5c4a1",
  light3 = "#000000", --"#bdae93",
  light4 = "#000000", --"#a89984",
  bright_red = "#000000", --"#fb4934",
  bright_green = "#b8bb26", --green keywords funcs etc
  bright_yellow = "#fabd2f", -- mode font color in the bar 
  bright_blue = "#83a598", -- variables and suggestions part color
  bright_purple = "#d3869b",
  bright_aqua = "#8ec07c",
  bright_orange = "#fe8019",
  neutral_red = "#cc241d",
  neutral_green = "#98971a",
  neutral_yellow = "#d79921",
  neutral_blue = "#458588",
  neutral_purple = "#b16286",
  neutral_aqua = "#689d6a",
  neutral_orange = "#d65d0e",
  faded_red = "#9d0006",
  faded_green = "#79740e",
  faded_yellow = "#b57614",
  faded_blue = "#076678",
  faded_purple = "#8f3f71",
  faded_aqua = "#427b58",
  faded_orange = "#af3a03",
  gray = "#928374",
  },
  dim_inactive = true,
  transparent_mode = true,
})
vim.cmd([[colorscheme gruvbox]])
cmd("syntax off")

-- TreeSitter
require'nvim-treesitter.configs'.setup {
  -- A list of parser names, or "all" (the five listed parsers should always be installed)
  ensure_installed = { "c", "lua", "vim", "vimdoc", "cpp", "go" },

  -- Install parsers synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- Automatically install missing parsers when entering buffer
  -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
  auto_install = true,

  ---- If you need to change the installation directory of the parsers (see -> Advanced Setup)
  -- parser_install_dir = "/some/path/to/store/parsers", -- Remember to run vim.opt.runtimepath:append("/some/path/to/store/parsers")!

  highlight = {
    enable = false,

    -- NOTE: these are the names of the parsers and not the filetype. (for example if you want to
    -- disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is
    -- the name of the parser)
    -- list of language that will be disabled
    disable = { "c", "go" },
    -- Or use a function for more flexibility, e.g. to disable slow treesitter highlight for large files
    --[[disable = function(lang, buf)
        local max_filesize = 100 * 1024 -- 100 KB
        local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
        if ok and stats and stats.size > max_filesize then
            return true
        end
	]]--

   -- end,

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
}
-- Lsp
lspconfig.tsserver.setup({})
lspconfig.eslint.setup({})
