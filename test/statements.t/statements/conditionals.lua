if true then
	print("inside true", nil)
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

print("inside:", inside)
