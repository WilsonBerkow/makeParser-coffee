# (c) Wilson Woodcock Berkow. MIT License.

# (This is parse0.2.coffee in my local folder. This is the first commit of this file.)

"use strict"

typeOf = makeParser.util.typeOf
getWhite = makeParser.getWhite
cons = (x, xs) -> [x, xs...]
head = (xs) -> xs[0]
tail = (xs) -> xs.slice(1)
last = (xs) -> xs[xs.length - 1]
concat = (xs, ys) -> xs.concat(ys)
boxJson = (x) ->
    if typeOf(x) is "object"
        obj = {}
        for name, val of x
            obj[name] = boxJson(val)
        {"type": "object", "properties": obj}
    else if typeOf(x) is "array"
        arr = []
        for val in x
            arr.push boxJson(val)
        {"type": "array", "elements": arr}
    else
        {"type": typeOf(x), "val": x}
getSpaces = makeParser "getSpaces", /^[ ]+/

findRecentestCharIndex = (str, index, char) ->
    makeParser.util.loop (repeat) => (i = index) =>
        if str[i] is undefined
            -1
        else if str[i] is char
            i
        else
            repeat(i - 1)
distToPrevNewline = (str, i) ->
    startI = findRecentestCharIndex(str, i, "\n") + 1
    i - startI
indentsAbstraction = (which) ->
    tester = (fstIndent, nextIndent) ->
        if typeOf(which) is "string"
            switch which
                when "in"   then (nextIndent > fstIndent)
                when "same" then (nextIndent is fstIndent)
                when "out"  then (nextIndent < fstIndent)
        else if typeOf(which) is "number"
            (nextIndent - fstIndent) is which # Here `which` is the amount of indent. For outdent, its negative.
        else
            throw new Error "Invalid argument to indentsAbstraction: #{which}"
    makeParser -> # THIS NEEDS TO TO MANUAL ANALYSIS OF THE STRING: assume its at the end of a line, check the indent on the current line, check difference between indent on next line.
        # This must be used at the END of the line, before the \n and before the line that you want indented.
        @require "\n"
        fstLine = @str[findRecentestCharIndex(@str, @index - 2, "\n") + 1...@index - 1] # 2 is subtracted from the arg to findRecen...(...) so that it doesnt stop at the "\n" just @required().
        nextLine = @str[@index...@str.indexOf("\n", @index)]
        fstIndent = getSpaces.opt()(fstLine)?.length ? 0
        nextIndent = getSpaces.opt()(nextLine)?.length ? 0
        if tester(fstIndent, nextIndent)
            @require getSpaces
            nextIndent - fstIndent # For convenience, it returns the size of the dent.
        else
            throw new Error "Expected #{which}dent."
getIndent = indentsAbstraction("in").makeNamed("getIndent")
getSameDent = indentsAbstraction("same").makeNamed("getSameDent")
getterExactDent = (num) -> makeParser "getExactDent#{num}", -> # I think this will be useful for blocks which have to detect if the outdent at the end of a sub-block makes the next stmts at the same indent as the block, or ends the block.
    @require "\n"
    spaces = @optional(getSpaces)?.length ? 0
    @reqBool (spaces is num), "Incorrectly sized indent (from getterExactDent(#{num})). Spaces found: #{spaces}"
    spaces
getName = makeParser "getName", /^[A-Za-z][A-Za-z_0-9]*/
getUnit = makeParser "getUnit", getName
getValue = makeParser "getValue", [ # This has to be extended for variables and numbers with units.
    makeParser.seq(makeParser.jsonParsers.getNumber, getUnit).return(([num, unit]) -> {"type": "number+unit", "numeric": num, "unit": unit})
    makeParser.jsonParsers.getJson.return(boxJson)
    makeParser("'").then(getName).return(boxJson)
]

# getAssig. e.g. padding: 0.5em
getAssig = makeParser.seq(getName, ":", getWhite, getValue).makeNamed("getAssig").return ([name, _0, _1, val]) -> {"type": "assignment", "name": name, "val": val}

getIndentedBlock = do ->
    getStmts = makeParser "getStmts", ->
        indentSize = distToPrevNewline @str, @index # NOTE: It doesn't require that the dist be occupied by spaces... i should definately change that.
        stmt = @require [getAssig, getPrefixedBlock]
        @optional getterExactDent(indentSize),
            found: => cons stmt, @require(getStmts)
            notFound: => [stmt]
    getIndent.then(getStmts).makeNamed("getIndentedBlock")

# getNamedContentSelector gets the "> NAME" pattern (nothing after that though).
getNamedContentSelector = makeParser(">").then(getSpaces).then(getName).return (name) -> {"type": "namedContentSelector", "val": name}
getAttrsSelector = makeParser "getAttrsDescription", do ->
    getAttrName = makeParser "getAttrName", getName
    getAttrPair = makeParser.seq(getAttrName, getSpaces, "=", getSpaces, makeParser.jsonParsers.getString).return((name, sp1, eq, sp2, val) -> {"attr": name, "expectedVal": val})
    makeParser.many([getAttrPair, getAttrName], start: "@(", sep: makeParser(",").then(getSpaces), end: ")")
getClassSelector = makeParser "getClassSelector", getName

# All of these "slash" things are about the form "/ childtype / @(...)..." stuff. / at the start of a line is equivalent to "Type"
withSlash = (prsr) -> makeParser.seq("/", getSpaces, prsr).return last
optSlash = (prsr) -> makeParser (prsr.name + "--optSlash"), [prsr, withSlash prsr]
spacesFirst = (prsr) -> makeParser (prsr.name + "--spacesFirst"), getSpaces.then(prsr)
map = (fn, arr) -> arr.map fn
getSelector = makeParser.many(map(spacesFirst, [getNamedContentSelector, optSlash getAttrsSelector]),
                              start: [getNamedContentSelector, withSlash(getAttrsSelector), withSlash(getClassSelector)])

# getMixinParams. e.g. (foo, bar, baz)
getMixinParams = makeParser.many(getName, start: "(", sep: makeParser(",").then(getWhite.opt()), end: ")")

# getBlockPrefix.
prefixWrap = ([prefixName, _, prefixArg]) -> {"type": "prefix", "which": prefixName, "arg": prefixArg}
getBlockPrefix = makeParser "getBlockPrefix", [
    makeParser.seq("Type", getSpaces, getName).return(prefixWrap),
    makeParser.seq("When", getSpaces, getName).return(prefixWrap),
    makeParser.seq("Mixin", getSpaces, getName, getMixinParams).return(([_0, _1, name, params]) -> {"type": "prefix", "which": "Mixin", "prefixArgs": [name], "mixinParams": params}),
    getNamedContentSelector
]

getPrefixedBlock = makeParser.seq(getBlockPrefix, ":", getIndentedBlock).makeNamed("getPrefixedBlock").return ([prefix, _, blockStmts]) -> {"type": "block", "prefix": prefix, "stmts": blockStmts}

# getAssigArgs. e.g. (width: 3, height: 9)
getAssigArgs = makeParser.many(getAssig, sep: [makeParser(",").opt().then(getSameDent), makeParser(",").then(getSpaces)])
invocFuncs = [{name: "Rect",    args: ["width", "height"]},
              {name: "Ellipse", args: ["x_radius", "y_radius"]},
              {name: "Circle",  args: ["radius"]},
              {name: "Url",     args: ["url"]}]
getInvocation = makeParser "getInvocation", invocFuncs.map (o) -> # Should this be changed to a single function with a loop, so there aren't unnecessarily many of a similar function? This shouldn't really matter much.
    #stmtGetters = o.args.map((prop) -> makeParser.seq(prop, ":", getWhite, getValue))
    makeParser.seq(o.name, [ # These options are all argument-forms:
        # Next is: e.g. Rect(width: X, height: Y)
        makeParser.seq("(", getAssigArgs, ")").makeNamed("get-invoc-with-parens").return(([_0, assigs, _1]) -> assigs)
        
        # Next is: Like previous, but without parens. e.g. Rect width: X, height: Y
        makeParser.seq(getWhite, getAssigArgs).makeNamed("get-invoc-w/o-parens").return(([wh, assigs]) -> assigs)
        
        # Next is: Like previous one, but NO NAMES, and arguments can have ANY whitespace between them (INCLUDING newlines). e.g. Rect X, Y
        makeParser.many(getValue, start: getWhite, sep: makeParser.seq(",", getWhite))
        
        # TODO: the following will never run, as the previous will catch it, but i dont have time to work on it now. Do that later tho.
        # Next is: With most whitespace: statements can be on multiple line, appearing on a line after function name
        makeParser.seq(getIndent, getAssigArgs).makeNamed("get-invoc-args-on-mult-lines").return(([ind, assigs]) -> assigs)
    ]).makeNamed("get-options-for-arguments-forms").return (findings) -> {name: findings[0], args: findings[1], src: findings.src}
    # TODO: SOLVE: somewhere, there is a conditional based on .parserName that shouldn't be there, cuz when the above seq-parser has .parserName, the error message works, and reads the names of the functions in the above array, but when it doesn't have a name, it read's their names as `undefined`....

getStylesheet = makeParser.many(getPrefixedBlock, sep: getWhite, end: getWhite.opt()).return (data) -> {"type": "stylesheet", "body": data}
# What if getStylesheet just was getStmts, and top-level assignments/properties would apply to the document root element... (which would be similar to body for the page, and to the root elem for widgets)?
