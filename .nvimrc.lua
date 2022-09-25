local keymap = vim.api.nvim_set_keymap
local buf_keymap = vim.api.nvim_buf_set_keymap

local Terminal = require('toggleterm.terminal').Terminal

local buildterm = Terminal:new({
  direction = "float",
  float_opts = {
    border = "curved",
  },
  -- function to run on opening the terminal
  on_open = function(term)
    buf_keymap(term.bufnr, "n", "q", "<cmd>:q!<CR>", { noremap = true, silent = true })
    buf_keymap(term.bufnr, "t", "<C-q>", "<cmd>:q!<CR>", { noremap = true, silent = true })
  end,
})


function _DUNEBUILD()
  buildterm:toggle()
  buildterm:send("dune build")
end

function _DUNERUN()
  buildterm:toggle()
  buildterm:send("dune exec oclox")
  buildterm:set_mode("i")
end

keymap("n", "<leader>bb", "<cmd>lua _DUNEBUILD()<CR>", { noremap = true, silent = true })
keymap("n", "<leader>rr", "<cmd>lua _DUNERUN()<CR>", { noremap = true, silent = true })
