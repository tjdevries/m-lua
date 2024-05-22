print "for 1, 10"
for i = 1, 10 do
  print(i)
end

print "for 1, 10, 1.5"
for i = 1, 10, 1.5 do
  print(i)
end

print "for 10, 1, -1"
for i = 10, 1, -1 do
  print(i)
end

print "break"
local sum = 0
for i = 1, 100, 1 do
  sum = sum + i
  if i >= 3 then
    break
  end
end
print("broken sum:", sum)

local x = function()
  for i = 1, 10 do
    if i == 5 then
      return i
    end
  end

  return "<this is an error>"
end
print("should be 5", x())

print "loop with exprs"
local my_func = function(v)
  return v + 1
end
for i = my_func(1), 4 do
  print(i)
end
