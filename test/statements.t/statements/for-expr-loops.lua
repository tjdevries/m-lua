print "starting expr loops"
local t = {
  [-1] = "skipped",
  [0] = "KEKW IMAGINE STARTING AT 0 NOT IN C",
  "a",
  "b",
  "c",
  true,
  false,
  [6] = "wow",
  [7] = {},
}
for i, v in ipairs(t) do
  print("expr-loop:", i, v)
end
print "ending expr loop"
