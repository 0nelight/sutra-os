-- The editor in media/sdk/*.gif: stock Neovim (0.11+) and sutra's one language server
-- (clients/ent-lsp), which runs pyright for Python and names completions in the model's words.
vim.o.background = "light"
vim.o.termguicolors = true
vim.cmd.colorscheme("default")
vim.api.nvim_set_hl(0, "Normal", { fg = "#1f2328" })
vim.api.nvim_set_hl(0, "LspSignatureActiveParameter", {})
vim.o.fillchars = "eob: "
vim.o.number = true
vim.o.signcolumn = "no"
vim.o.laststatus = 0
vim.o.ruler = false
vim.o.showmode = false
vim.o.shortmess = vim.o.shortmess .. "IcF"
vim.o.swapfile = false
vim.o.expandtab = true
vim.o.shiftwidth = 4
vim.o.completeopt = "menu,menuone,noinsert,popup"
vim.o.pumheight = 10

vim.lsp.config("sutra", {
  cmd = { "bb", "--config", os.getenv("ENT_LSP_CONFIG"), "lsp" },
  filetypes = { "python" },
  root_markers = { "pyrightconfig.json" },
  settings = {
    python = { analysis = { diagnosticMode = "openFilesOnly", useLibraryCodeForTypes = true } },
  },
})
vim.lsp.enable("sutra")

local shown = { min = vim.diagnostic.severity.WARN }
vim.diagnostic.config({
  virtual_text = false,
  virtual_lines = { severity = shown, format = function(d) return d.message end },
  underline = { severity = shown },
  signs = false,
  update_in_insert = false,
})

local function signature()
  vim.schedule(function()
    vim.lsp.buf.signature_help({ border = "rounded", focusable = false, anchor_bias = "below" })
  end)
end

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(ev)
    vim.lsp.completion.enable(true, ev.data.client_id, ev.buf, {
      autotrigger = true,
      convert = function(item)
        local model = item.labelDetails and item.labelDetails.description
        return model and { kind = model } or {}
      end,
    })
    vim.api.nvim_create_autocmd("InsertCharPre", {
      buffer = ev.buf,
      callback = function()
        if vim.v.char == "(" then signature() end
      end,
    })
  end,
})
