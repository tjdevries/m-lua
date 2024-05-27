local t = {}
local function something()
  return t
end

something()[1] = 1
something()[2] = 2
something()[3] = 3

print(vim.inspect(t))
