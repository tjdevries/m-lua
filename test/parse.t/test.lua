-- stylua: ignore start
print("h", "hello world")
print("line 2")
print("escaped \" ")
print('escaped \' ')
print(5 * 5.01 + 37 * 2)
print({ 1 })
print({ 1, 2, 3 })
print(tostring(1))
print(tostring({}))

local x = "hi LLL - 1 is very based"
print(x)

local y = 10
local z = 15
print("first", y, "+", z, "=", y + z)

do
  local abc = 10
  print("in a do block", abc, "(", y, z, ")")
end

if true then
  print("truthful")
end

if false then
  print("should not print")
elseif false then
elseif false then
else
  print("in else block")
end

local inside = false
if true then
  inside = true
end

print('inside:', inside)

print("again", y, "+", z, "=", y + z)
print(abc)

-- _G.print("hi")

-- local count = 0
-- for i = 1, 10000000 do
--   count = count + i
-- end
-- print(count)

-- local x = 5
-- print(x)

-- stylua: ignore end
