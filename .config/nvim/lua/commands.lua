-- mason, write correct names only

vim.api.nvim_create_user_command("MasonInstallAll", function()
  vim.cmd "MasonInstall css-lsp html-lsp lua-language-server typescript-language-server stylua prettier"
end, {})

-- Automatic tex files compile
vim.api.nvim_create_autocmd("FileType", {
  pattern = "tex",
  callback = function()
    vim.schedule(function()
      vim.cmd("VimtexCompile")
    end)
  end,
})

