local cmp = require("cmp")
local luasnip = require("luasnip")

if vim.g.snippet_directory then
  require("luasnip.loaders.from_vscode").lazy_load({
    paths = { vim.g.snippet_directory }
  })
end

luasnip.config.set_config({
  history = true,
  updateevents = "TextChanged,TextChangedI",
  enable_autosnippets = true
})

cmp.setup({
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = {
    -- https://github.com/hrsh7th/nvim-cmp/wiki/Example-mappings#super-tab-like-mapping
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, {"i", "s"}),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { "i", "s" }),
  },
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
  })
})

local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())

local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '<space>dn', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', '<space>dp', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  -- vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap=true, silent=true, buffer=bufnr }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'gt', vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set('n', '<space>f', vim.lsp.buf.format, bufopts)
  vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
  vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
  vim.keymap.set('n', '<space>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, bufopts)
  vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
  vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
  vim.keymap.set('n', '<space>f', function() vim.lsp.buf.format { async = true } end, bufopts)
end

local lsp_flags = {
  -- This is the default in Nvim 0.7+
  debounce_text_changes = 150,
}

local language_servers = {
  clangd = {},
  pyright = {}, -- pyright
  -- tsserver = {}, -- typescript-language-server
  cssls = {},
  elmls = {}, -- elm-language-server
  gopls = {}, -- gopls
  denols = {}, -- deno built in
  bashls = {}, -- bash-language-server
  lua_ls = {
    Lua = {
      runtime = {
        version = 'LuaJIT',
      },
      diagnostics = {
        globals = {'vim'},
      },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true)
      },
      telemetry = {
        enable = false,
      }
    },
  },
  hls = {}, -- haskell-language-server
  html = {}, -- vscode-langservers-extracted
  jsonls = {}, -- vscode-langservers-extracted
  lemminx = {}, -- lemminx (for xml)
  nil_ls = {
    ['nil'] = {
      formatting = {
        command = { "nixfmt" },
      },
    },
  }, -- github:oxalica/nil
  dhall_lsp_server = {}, -- dhall-lsp-server
  -- rnix = {}, -- rnix-lsp
  jqls = {}, -- jq-lsp
  rust_analyzer = { ["rust-analyzer"] = {} },
  -- eslint = {},
  -- volar? vls?
  texlab = {
    texlab = {
      auxDirectory = ".",
      bibtexFormatter = "texlab",
      build = {
        args = { "-pdf", "-interaction=nonstopmode", "-synctex=1", "%f" },
        executable = "latexmk",
        forwardSearchAfter = false,
        onSave = false
      },
      chktex = {
        onEdit = false,
        onOpenAndSave = false
      },
      diagnosticsDelay = 300,
      formatterLineLength = 80,
      forwardSearch = {
        args = {}
      },
      latexFormatter = "latexindent",
      latexindent = {
        modifyLineBreaks = false
      }
    }
  }
}

for server, settings in pairs(language_servers) do
  vim.lsp.config(server, {
    on_attach = on_attach,
    flags = lsp_flags,
    settings = settings,
    capabilities = capabilities
  })
  vim.lsp.enable(server)
end
