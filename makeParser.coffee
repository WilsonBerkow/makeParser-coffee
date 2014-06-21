# (c) Wilson Woodcock Berkow. MIT License.

# parser0.6.coffee

"use strict"
util = Object.freeze # This is frozen as it is publicly added to makeParser.
    "y": (le) -> # Y-Combinator
        ((f) ->
            f(f)
        ) (f) ->
            le (args...) ->
                f(f).call(this, args...)
    "loop": (f) -> util.y(f)() # For functional/expressional loops. Interface: util.loop (repeat) => => ...
    "makeObject": (proto, ownProps) ->
        result = Object.create proto
        for prop, val of ownProps
            if ownProps.hasOwnProperty prop
                result[prop] = val
        result
    "typeOf": (x) ->
        if Array.isArray(x) then "array"
        else if Object.prototype.toString.call(x) is "[object RegExp]" then "regexp"
        else if x is null then "null"
        else `typeof x`
cons = (x, xs) -> [x, xs...]
head = (xs) -> xs[0]
tail = (xs) -> xs.slice(1)
last = (xs) -> xs[xs.length - 1]
empty = (l) -> l.length is 0
concat = (xs, ys) -> xs.concat(ys)

parsingError = (msg) -> {"name": "Parsing Error", "msg": msg}
isParsingError = (err) -> err?.name is "Parsing Error"
makeParsingInstance = do -> # The parsing instance contains methods for parsing, and the information for parsing (e.g. source being parsed and index/position in it).
    isParser = (x) -> util.typeOf(x) is "function" and x.isParser
    makeSureParser = (x) -> if isParser(x) then x else makeParser(x) # This must be used here to prevent infinite recursion between @require and makeParser, but does not need to be used anywhere else.
    parserUsage = (which) -> # This is the abstraction for @optional and @require
        (a, b, c) ->
            # Handle all arrangements of arguments:
            #  (The arrangements are (lookFor, found, notFound)
            #                        (lookFor, {found, notFound, dontAdvance, args})
            #                        ({lookFor, found, notFound, dontAdvance, args}))
            if util.typeOf(a) in ["number", "null", "undefined"]
                @throw "Invalid argument to @#{which}."
            if util.typeOf(a) is "object"
                prsr = makeSureParser a.lookFor
                other = a
            else if util.typeOf(b) is "object"
                prsr = makeSureParser a
                other = b
            else
                prsr = makeSureParser a
                other = {
                    found: b
                    notFound: c
                    args: []
                }
            if which is "optional"
                other.notFound ?= -> # This makes nothing happen if the thing isnt found.
            prsr.requireFor(@, other)
    proto = Object.freeze
        # FIRST ARE THE UTIL-LIKE FUNCTIONS:
        "loop": util.loop
        "beginsWith": (s) -> # TODO: test @test so that it can be used instead.
            if util.typeOf(@str.startsWith) is "function"
                @str.slice(@index).startsWith(s)
            else
                @loop (repeat) => (i = @index) =>
                    if @str.slice(0, i + 1) is s
                        true
                    else
                        repeat(i + 1)
        "char": (x = 0) -> @str[@index + x] # Gets the current char, or one before/after it depending on -x-.
        "soFar": -> @str[@startIndex...@index]
        "until": (x) -> if util.typeOf(x) is "string" then @str[@index...@str.indexOf(x, @index)] else @str[@index...(@index + x)]
        # NOW THE PARSING FUNCTIONS:
        "test": (prsr) ->
            !!(prsr.opt()(@str, startAt: @index))
        "advance": (x) ->
            @index += switch util.typeOf x
                when "string" then x.length
                when "number" then x
                when "undefined" then 1
        "advancePast": (s) -> # Consider removing this. I have never had a need for it and do not see one.
            while not @beginsWith(s)
                @advance()
            @advance(s)
        "throw": (msg) ->
            throw parsingError msg
        "reqBool": (bool, err) ->
            if !bool
                throw parsingError(err ? "Test to @reqBool failed.")
        "caseParse": (o, onNoMatch) -> # Each key of -o- is a string to optionally parse.
            @loop (repeat) => (keys = Object.keys o) =>
                if empty(keys)
                    if onNoMatch then onNoMatch()
                    else @throw("Expected one of the following: " + JSON.stringify(Object.keys(o), null, 4))
                else
                    fn = o[head(keys)]
                    @optional head(keys),
                        found:    => if fn.isParser then @require fn else fn() # TODO: Should it always do @require(fn), never just fn()?
                        notFound: => repeat tail keys
        "white": (req) ->
            if req is "req"
                @require makeParser.getWhite
            else
                @optional makeParser.getWhite
        "optional": parserUsage("optional")
        "require": parserUsage("require")
    (str, i = 0) ->
        util.makeObject(proto, {
            "index": i
            "str": str
            "startIndex": i
        })
parserListToString = (arr) -> # For stringifying an array of parser options as to display the name of each parser.
    util.loop (repeat) => (options = arr, str = "[") =>
        if empty options
            str.slice(0, -2) + "]" # The slice gets rid of the ", " at the end.
        else
            x = head options
            switch util.typeOf(x)
                when "function"
                    if x.isParser
                        repeat(tail(options), str + "(a parser named #{x.parserName}), ")
                    else
                        repeat(tail(options), str + "(a plain function), ")
                when "regexp"
                    repeat(tail(options), str + x.toString() + ", ")
                when "undefined"
                    repeat(tail(options), str + "undefined, ")
                else
                    repeat(tail(options), str + JSON.stringify(x) + ", ")
@makeParser = (args...) ->
    # Handle args combos:
    if args.length > 1
        name = args[0]
        x = args[1]
    else if args.length is 1
        x = args[0]
    else
        throw new Error("makeParser requires arguments")
    ### Overloads of x in -makeParser-:
        string:          x is the string to be required
        array:           x is a list of options, where one must match. Earlier elements have higher precedence.
        regexp:          if the string doesn't match x, it throws, otherwise x.exec(@str) is skipped returned.
        parser-function: x is just returned
        plain function:  x is used to make a parser-function. (Returns a NEW function that is a parser
                          function which USES the input function by applying it to an instance. It also
                          sets some methods (requireFor, ...) and sets .isParser to true).
    ###
    # Handles all overloads:
    if util.typeOf(x) is "string" # This is the simplest overload: just require it (and advance past it). RETURNS the string.
        makeParser (name ? "string-form-parser: #{JSON.stringify x}"), ->
            if @beginsWith x
                @advance x.length # (Can't do @require(x) here because x is a string and this is the def of @require(x) for strings).
                @soFar()
            else
                @throw """Expected "#{x}" at index #{@index} and instead found #{@char()} in string:\n#{JSON.stringify(@str)}"""
    else if util.typeOf(x) is "array" # Each element of the array is an OPTION, and this requires one to match. RETURNS whatever the matched option returns.
        makeParser (name ? "array-form-parser: #{parserListToString x}"), (args) ->
            @loop (repeat) => (i = 0) =>
                if not x[i]?
                    @throw "(From #{name}) Expected one of the following: #{parserListToString(x)} in string:\n#{@str}\nat index #{@index}"
                @optional x[i],
                    args: args
                    notFound: ->
                        repeat(i + 1)
    else if util.typeOf(x) is "regexp"
        makeParser (name ? "regexp-form-parser: #{x}"), ->
            val = x.exec @str[@index...]
            if val is null
                @throw "Expected the regexp pattern " + x.toString() + " in string ``#{@str}'' at index #{@index}"
            else
                @require val[0]
    else if util.typeOf(x) is "function" # This is the primary form, which every other overload is defined in terms of.
        if x.isParser
            makeParser (name ? "copy-of-parser: #{name}"), ->
                @require x
        else # This is the usual case. -x- is a function intended to be made into a parser.
            ### The parser (in the variable -parser-), can have the following arrangments of arguments:
                    1. "string"
                    2. ({string, startAt, args, found, notFound})
                    3. (string, {startAt, args, found, notFound})
                    4. (string, startAt, {args, found, notFound})
                    5. (string, startAt, args, {found, notFound})
                    6. (string, startAt, args, found, notFound)
            ###
            useParserAbstraction = (callback) ->
                (args...) ->
                    if args.length is 1
                        if util.typeOf(args[0]) is "string"
                            str = args[0]
                        else
                            str = args[0].lookFor
                            startAt = args[0].startAt
                            other = args[0]
                    else if args.length is 2
                        str = args[0]
                        if util.typeOf(args[1]) is "object"
                            startAt = args[1].startAt
                            other = args[1]
                        else
                            startAt = args[1]
                    else if args.length is 3
                        str = args[0]
                        startAt = args[1]
                        other = args[2]
                    callback(str, startAt, other)
            parser = useParserAbstraction (str, startAt, other) ->
                parser.requireFor(makeParsingInstance(str, startAt), other)
            parser.wholeStr = useParserAbstraction (str, startAt, other) ->
                parsingInstance = makeParsingInstance(str, startAt)
                result = parser.requireFor(parsingInstance, other)
                if parsingInstance.index isnt parsingInstance.str.length
                    throw parsingError("Expected end of string index #{parsingInstance.index}.")
                else
                    result
            parser.requireFor = (origInstance, other = {}) ->
                if util.typeOf(other.notFound) is "function"
                    # This branch is effectively the @optional function.
                    instance = makeParsingInstance(origInstance.str, origInstance.index)
                    try
                        val = x.call(instance, other.args)
                    catch e
                        if isParsingError e
                            err = e
                        else
                            throw e
                    if err
                        other.notFound(err)
                    else
                        if not other.dontAdvance
                            origInstance.index = instance.index # This is what synchronizes the referenced instance with the one it's used in, so that @require()ing another function also advances @index in the current instance.
                        if other.found
                            other.found(val)
                        else
                            val
                else
                    instance = makeParsingInstance(origInstance.str, origInstance.index)
                    val = x.call(instance, other.args)
                    if not other.dontAdvance
                        origInstance.index = instance.index
                    if other.found
                        other.found(val)
                    else
                        val
            parser.makeNamed = (name) ->
                newPrsr = makeParser name, (args) ->
                    @require parser, args: args
            parser.return = (fn) ->
                makeParser parser.parserName, (args) ->
                    @require parser, args: args, found: fn
            parser.then = (x) -> # Note: This returns the result of the LAST parser (e.g. in getFoo.then(getBar).then(getBaz)), while makeParser.seq returns a list of the results.
                makeParser ->
                    @require parser
                    @require x
            parser.opt = ->
                makeParser (parser.parserName + "--opt"), ->
                    @optional parser
            parser.isParser = true
            parser.parserName = name
            Object.freeze parser
    else
        throw new Error("The -makeParser- function requires argument(s).")
makeParser.util = util
makeParser.seq = (xs...) ->
    makeParser ->
        findings = []
        @loop (repeat) => (i = 0) =>
            if i < xs.length
                findings.push(@require xs[i])
                repeat(i + 1)
            else
                findings.src = @soFar()
                findings
makeParser.many = (x, other) -> # TODO: test other.amt. TODO: CHANGE THE ARGS PROPERTY: make it take an ARRAY of args, not an object of args.
    if not x?
        throw new Error "Invalid first argument to makeParser.many: #{x}"
    if util.typeOf(other.amt) is "number" # TODO: Consider putting this case into another function, like -makeParser.several-, so it is very clear which is being used.
        parseInner = makeParser (args = {}) ->
            args.amtLeft ?= other?.amt 
            if args.amtLeft <= 0
                []
            else
                first = @require x
                rest = []
                if args.amtLeft is 1
                    [first]
                else
                    @optional ->
                        if other?.sep
                            @require other?.sep
                        rest = @require parseInner,
                            args:
                                amtLeft: args.amtLeft - 1
                    cons first, rest
        makeParser ->
            if other?.start
                @require other.start
            findings = @require parseInner
            if other?.end
                @require other.end
            findings
    else
        parseInner = makeParser -> # This parses it without the start- and end-sequence so that it can have a simple recursive structure.
            first = @require x
            rest = []
            @optional ->
                if other?.sep
                    @require other?.sep
                rest = @require parseInner
            cons first, rest
        makeParser ->
            if other?.start
                @require other.start
            findings = if other?.allowEmpty then @optional(parseInner) else @require(parseInner)
            if other?.end
                @require other.end
            findings ? [] # When elements are optional and not there, findings will be `undefined` as it is returned from the @optional(parserInner) invoc above.
makeParser.cases = (a, b) ->
    if util.typeOf(a) is "string"
        name = a
        o = b
    else
        o = a
    makeParser name, ->
        @caseParse(o)
makeParser.getWhite = makeParser "getWhite", /^[\s]+/
makeParser.jsonParsers = do -> # TODO: REALLY TEST ALL OF THESE INDIVIDUALLY AND CAREFULLY (I just (6/20/14) fixed a stupid, glaring would-be reference error). Also, CHANGE THE NAME. Its too long. make it makeParser.json
    getString = makeParser "getString", (args) ->
        @require '"'
        @loop (repeat) => =>
            if @char() is undefined
                @throw "Unterminated JSON string literal"
            if @char() is '"' and (@char(-1) isnt "\\" or @char(-2) is "\\") # This means that the string has been terminated.
                @advance()
                JSON.parse(@soFar())
            else
                @advance()
                repeat()
    getDigits = makeParser "getDigits", /^[\d]+/ # This returns the string of digits.
    getNumber = makeParser "getNumber", ->
        @optional ["+", "-"]
        @require getDigits
        @optional ->
            @require "."
            @require getDigits
            @optional ->
                @require "e"
                @optional ["+", "-"]
                @require getDigits
        JSON.parse(@soFar())
    getBoolean = makeParser "getBoolean", ->
        @caseParse
            "true": -> true
            "false": -> false
    getNull = makeParser("getNull", "null").return(-> null)
    getArray = do ->
        arrayBody = makeParser ->
            # This parses everything betweten the [ and ] in an array, and requires there to be at least one item.
            fst = @require getJson
            @white()
            @optional ",",
                found: => # Handles the rest of the elements.
                    @white()
                    rest = @require arrayBody
                    cons fst, rest
                notFound: => [fst] # This means -fst- is the only element.
        makeParser "getArray", ->
            @require "["
            @white()
            val = @optional arrayBody
            @white()
            @require "]"
            val ? []
    getObject = do ->
        getObjectPair = makeParser -> # Parses "PROP": VAL forms.
            prop = @require string
            @require ":"
            @white()
            val = @require getJson
            {"prop": prop, "val": val, "src": @soFar()}
        getObjectBody = makeParser ->
            @optional getObjectPair,
                found: (fst) =>
                    rest = [] # This is the default if there is nothing else in the object
                    @optional ->
                        @white()
                        @require ","
                        @white()
                        rest = @require getObjectBody
                    cons fst, rest
                notFound: => [] # No pairs in the list corresponds to an empty object.
        makeParser "getObject", ->
            # Validate structure and get data:
            @require "{"
            @white()
            pairs = @require getObjectBody
            @white()
            @require "}"
            # Construct object:
            obj = {}
            @loop (repeat) => (i = 0) =>
                if i < pairs.length
                    obj[pairs[i].prop] = pairs[i].val
                    repeat(i + 1)
            obj
    getJson = makeParser "getJson", [getNumber, getNull, getBoolean, getArray, getString, getObject]
    Object.freeze {
        "getJson":    getJson
        "getString":  getString
        "getNumber":  getNumber
        "getBoolean": getBoolean
        "getNull":    getNull
        "getArray":   getArray
        "getObject":  getObject
    }
Object.freeze makeParser

### TODO: Things to THINK ABOUT for the future:
 -Allowing some easier way of creating parsers like ones from makeParser
   out of parsers which are just plain functions (i.e. some functions
   which create makeParser-parsers from something like JSON.parse).
 -Making parsers be objects (not functions) with these methods:
    "atStart": Just run the parser (equivalent to what a parser is now)
    "wholeStr": Runs the parser and makes sure that the content occupies the ENTIRE string (and there is no shit at the end of the string).
   or at least having the latter feature.
 -Make the "args" property of the options object in @require and @optional calls be an array of args
    rather than an object to be passed as the first arg.
 -ADDITIONALLY: Should makeParser record a stack so that when this error
    function is called, you can expand the error and observe which parsers
    were called, without having to look at the actual stack and just observing
    a bunch of calls to anonymous function or to `parser` or `requireFor` and
    shit?
###
