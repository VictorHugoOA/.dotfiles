local telescope = require('telescope')
local builtin = require('telescope.builtin')
local action_state = require('telescope.actions.state')
local actions = require('telescope.actions')
local previewers = require("telescope.previewers")


telescope.load_extension('harpoon')
telescope.load_extension('fzf')
telescope.load_extension('file_browser')


telescope.setup {
    defaults = {
        prompt_position = "top",
        sorting_strategy = "ascending",
        prompt_prefix = "#> ",
        mappings = {
            i = {
                ["<c-o>"] = function() builtin.find_files() end,
                ["<c-e>"] = function(prompt_bufnr)
                        actions.file_edit(prompt_bufnr)
                end,
                ["<c-s"] = function (prompt_bufnr)
                    local selection = action_state.get_selected_entry()

                end,
            },
            n = {
                ["cd"] = function (prompt_bufnr)
                    local selection = action_state.get_selected_entry()
                    local dir = vim.fn.fnamemodify(selection.path, ":p:h")
                    actions.close(prompt_bufnr)
                    vim.cmd(string.format("silen cd %s", dir))
                end,
            }
        }
   }
}
