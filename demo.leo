#lang leo

use number
use text

choice
  the "foo"
  not something else
  not number
do switch
  it text
  it "something else"
  it number.text
