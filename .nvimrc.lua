local term = require('harpoon.term')


vim.keymap.set("n", "<leader>bb",
  function()
    term.sendCommand(1, "dune build\n")
    term.gotoTerminal(1)
  end,
  { noremap = true, silent = true }
)
vim.keymap.set("n", "<leader>rr",
  function()
    term.sendCommand(2, "dune exec oclox\n")
    term.gotoTerminal(2)
    vim.cmd("startinsert")
  end,
  { noremap = true, silent = true }
)
