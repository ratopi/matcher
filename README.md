# matcher - A stupid simple string matcher and evaluator
  
Evaluates expressions like

    matcher:eval({'=', <<"Albert">>, <<"Albert">>})
                
Expressions are tuples in one of the following form:

    { Op, Arg }
    { Op, Arg1, Arg2 }
    { Op, [Arg, ...] }

Expressions can be nested: Any argument can be an expression again:

    {'&', [{'=', <<"Albert">>, <<"Albert">>}, {'=', <<"Berta">>, <<"Albert">>}]}
       

`matcher:eval/2` allows to specify a `Provider`, which is a `fun/1` evaluates expression that can not be evaluated any further.

Example: If you have a map with atoms as keys, these keys can be replaces by its corresponding value.

    Map = #{ name => <<"Albert">> }.

    true = matcher:eval(Map, {'=', name, <<"Albert">>}). 
                                                        
`matcher:eval(Map, Expr)` is short for `matcher:eval(map_provider(Map), Expr)`.

You can use any `fun/1` as a provider.
BTW: If you like you can specify new operators with such a function, just handle something like `{my_cool_op, A, B}` in your function.


## List of Operators:

* One operand                
  * `!`, `not`
* Many operands (list)
  * `&`, `and`
  * `|`, `or`
* Two operands 
  * `<`, `lt` : less then
  * `<~`, `{lt, case_insensitive}` : less then (case insensitive)
  * `>`, `gt` : greater then
  * `>~`, `{gt, case_insensitive}` : greater then (case insensitive)
  * `>=`, `=>`, `le` : less then or equal
  * `>=~`, `=>~`, `{lt, case_insensitive}` : less then or equal (case insensitive)
  * `<=`, `=<`, `le` : less then or equal
  * `<=~`, `=<~`, `{lt, case_insensitive}` : less then or equal (case insensitive)
  * `=`, `==`, `eq` : equal
  * `=~`, `{eq, case_insensitive}` : equal (case insensitive) 
  * `?`, `part_of` : first string is part of second 
  * `?~`, `{part_of, case_insensitive}` : first string is part of second (case insensitive)
    
See ./test/matcher_SUITE.erl
         

## License

License is MIT ... but I like to hear from you, if you think this app is useful. :-)

## Source

http://github.com/ratopi/matcher
