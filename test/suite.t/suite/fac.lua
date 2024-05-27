local fac = function(n)
  local x = 1
  for i = 2, n do
    x = x * i
  end
  return x
end

print(fac(10))
assert(fac(10) == 3628800)
