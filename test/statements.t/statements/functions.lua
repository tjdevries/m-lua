---@diagnostic disable

local x = 5
local myfunc = function()
  do
    local x = "plz no"
  end
  x = 6
  do
    do
      if true then
        return x
      end
    end
  end
end
print(myfunc(), "|", x)

local shared = 5
local shared_func = function(shared, optional)
  return shared
end
print(shared_func(false))

local outside = true
local outside_func = function(outside)
  return outside
end
print(outside_func())

local additional_func = function(x)
  return x
end
print(additional_func(10, 11, 12))
