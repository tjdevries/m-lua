-- stylua: ignore start
-- Regular
f()
f("hello")
f "hello"
f({})
f {}
f { table = true }

-- Tables
t:name()
t:name("hello")
t:name "hello"
t:name({})
t:name {}
t:name { table = true }
-- stylua: ignore end
