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

print "starting pairs loop"
local map = {
  "first",
  "second",
  key = "value",
  ocaml = "based",
  teej_dv = "like & subscribe",
}
for k, v in pairs(map) do
  print("pairs-loop:", k, v)
end
print "ending pairs loop"

print "custom iterator"
local my_func = function(_, var)
  var = var + 1
  if var > 5 then
    return nil
  elseif var % 2 == 0 then
    return var, "even"
  else
    return var, "odd"
  end
end

for i, even_odd in my_func, nil, 0 do
  print(i, even_odd)
end
print "end custom iterator"
