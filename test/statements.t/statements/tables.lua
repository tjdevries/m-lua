local t = { "a", "b", "c" }
print("index t[1] -> 'a'", t[1])

local index = true
local other_index = false
local func_index = function() end
local string_table = {
  based = true,
  ocaml = "cool",
  ["a string"] = "wow",
  [index] = "berry true",
  [other_index] = "INCREDIBLE",
  [func_index] = "yes, functions can be keys",
}
print("based =", string_table.based)
print("ocaml is", string_table["ocaml"])
print("ocaml is also", string_table.ocaml)
print("this index is", string_table["a string"])
print("this index is also", string_table[index])
print("other index is", string_table[other_index])
print("with a literal", string_table[false])
print("func_index is", string_table[func_index])
