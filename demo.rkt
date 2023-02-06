#lang leo

null
push "Hello"
push "world"
push "!!!"
map string-length
filter applying: greater-than? 4
