:- use_module(library(http/json)).
:- use_module(library(md5)).
:- use_module(library(dicts)).

%%%%%%%%%%%%%%%%%%%%%%
%% Util predicates  %%
%%%%%%%%%%%%%%%%%%%%%%

filenameFromArgs([], 'testes/fib.json'):-!.
filenameFromArgs([Head|_], Head).

jsonParametersToList([], []).
jsonParametersToList([Head|Tail], [Head.get(text)|NewTail]) :-
    jsonParametersToList(Tail, NewTail).

verifyKind(JsonObject, SupposedKind) :-
    is_dict(JsonObject),
    get_dict(kind, JsonObject, SupposedKind).

bindParameters([], [], _, json{}).
bindParameters([Name|OtherParameters], [Value|OtherValues], Context, ParametersObject) :-
    bindParameters(OtherParameters, OtherValues, Context, ParametersObject1),
    evaluate(Value, Context, EvaluatedValue, _),
    string_to_atom(Name, NameAtom),
    put_dict(NameAtom, ParametersObject1, EvaluatedValue, ParametersObject).

different(X, X, false) :- !.
different(_, _, true).

to_str(String, String) :- string(String),!.
to_str([], "").
to_str([A|B], String) :-
    to_str(A, AS),
    to_str(B, BS),
    string_concat("[", AS, S1), 
    string_concat(S1, ",", S2),
    string_concat(S2, BS, S3),
    string_concat(S3, "]", String),!.
to_str(Something, String) :-
    string(String),
    (atom_string(Something, String),!);
    (number_string(Something, String),!).

join_arguments([], "_cached").
join_arguments([Arg|RemArgs], Result) :-
	join_arguments(RemArgs, Str),
	to_str(Arg, StrArg),
    string_concat(StrArg, Str, Result).

parse_args([], _, []).
parse_args([Argument|Arguments], Context, [ParsedArg|ParsedArgs]) :-
    evaluate(Argument, Context, ParsedArg, NewContext),
    parse_args(Arguments, NewContext, ParsedArgs).

hash_args(Arguments, Context, Hash) :-
    parse_args(Arguments, Context, ParsedArgs),
    join_arguments(ParsedArgs, Hash).

%%%%%%%%%%%%%%%%%%%%%%

file(FileCode, Code) :- get_dict(expression, FileCode, Code).

parameter(ParameterCode, Name) :- get_dict(text, ParameterCode, Name).

var(VarCode, Name) :- verifyKind(VarCode, "Var"), get_dict(text, VarCode, Name).

function(FunctionCode, Parameters, Value) :-
    verifyKind(FunctionCode, "Function"),
    get_dict(parameters, FunctionCode, JsonParameters),
    get_dict(value, FunctionCode, Value),
    jsonParametersToList(JsonParameters, Parameters).

functionCall(CallCode, Callee, Arguments) :-
    verifyKind(CallCode, "Call"),
    get_dict(callee, CallCode, Callee),
    get_dict(arguments, CallCode, Arguments).

letExpression(LetExpression, VarName, Value, Next) :-
    verifyKind(LetExpression, "Let"),
    get_dict(value, LetExpression, Value),
    get_dict(next, LetExpression, Next),
    get_dict(name, LetExpression, ParameterObject),
    parameter(ParameterObject, VarName).

printExpr(PrintExpression, Value) :-
    verifyKind(PrintExpression, "Print"),
    get_dict(value, PrintExpression, Value).

str(StrCode, Value) :- 
    verifyKind(StrCode, "Str"), get_dict(value, StrCode, Value).

intvar(IntCode, Value) :- 
    verifyKind(IntCode, "Int"), get_dict(value, IntCode, Value).

bool(BoolCode, Value) :-
    verifyKind(BoolCode, "Bool"), get_dict(value, BoolCode, Value).

if(IfCode, Condition, Then, Otherwise) :-
    verifyKind(IfCode, "If"),
    get_dict(condition, IfCode, Condition),
    get_dict(then, IfCode, Then),
    get_dict(otherwise, IfCode, Otherwise).

tuple(TupleCode, [First, Second]) :-
    verifyKind(TupleCode, "Tuple"),
    get_dict(first, TupleCode, First),
    get_dict(second, TupleCode, Second).

binary(BinaryOpCode, Operation, LeftOperand, RightOperand) :-
    verifyKind(BinaryOpCode, "Binary"),
    get_dict(lhs, BinaryOpCode, LeftOperand),
    get_dict(op,  BinaryOpCode, Operation),
    get_dict(rhs, BinaryOpCode, RightOperand).

firstExpression(FirstCode, First) :-
    verifyKind(FirstCode, "First"),
    get_dict(value, FirstCode, First),!.

secondExpression(SecondCode, Second) :-
    verifyKind(SecondCode, "Second"),
    get_dict(value, SecondCode, Second),!.

first([First,_], First) :- !.

second([_, Second], Second) :- !.

operate(LeftOperand, "Add", RightOperand, Result) :-
    number(LeftOperand),
    number(RightOperand),
    Result is LeftOperand + RightOperand,!.
operate(LeftOperand, "Add", RightOperand, Result) :-
    to_str(LeftOperand, LeftString),
    to_str(RightOperand, RightString),
    string_concat(LeftString, RightString, Result),!.
operate(LeftOperand, "Sub", RightOperand, Result) :-
    Result is LeftOperand - RightOperand,!.
operate(LeftOperand, "Mul", RightOperand, Result) :-
    Result is LeftOperand * RightOperand,!.
operate(LeftOperand, "Div", RightOperand, Result) :-
    Result is LeftOperand / RightOperand,!.
operate(LeftOperand, "Rem", RightOperand, Result) :-
    Result is LeftOperand rem RightOperand,!.
operate(LeftOperand, "Eq", LeftOperand, true):- !.
operate(_, "Eq", _, false):-!.
operate(LeftOperand, "Neq", RightOperand, Result) :-
    different(LeftOperand, RightOperand, Result),!.
operate(LeftOperand, "Lt", RightOperand, Result) :-
    (
       LeftOperand < RightOperand
    -> Result = true 
    ;  Result = false),!.
operate(LeftOperand, "Gt", RightOperand, Result) :-
    (
       LeftOperand > RightOperand
    -> Result = true 
    ;  Result = false),!.
operate(LeftOperand, "Lte", RightOperand, Result) :-
    (
       LeftOperand =< RightOperand
    -> Result = true 
    ;  Result = false),!.
operate(LeftOperand, "Gte", RightOperand, Result) :-
    (
        LeftOperand >= RightOperand
    -> Result = true 
    ;  Result = false),!.
operate(true, "And", true, true):-!.
operate(true, "And", false, false):-!.
operate(false, "And", true, false):-!.
operate(false, "And", false, false):-!.
operate(true, "Or", true, true):-!.
operate(true, "Or", false, true):-!.
operate(false, "Or", true, true):-!.
operate(false, "Or", false, false):-!.

get_cache_key(FunctionName, Context, Arguments, CacheKey) :-
    hash_args(Arguments, Context, JoinedArgumentsStr),
    string_concat(FunctionName, JoinedArgumentsStr, Args),
    atom_string(CacheKey, Args).

cached(FunctionName, Context, Arguments, Result) :-
    get_cache_key(FunctionName, Context, Arguments, CacheKey),
    get_dict('__cache__', Context, Cache),
    get_dict(FunctionName, Cache, CachedFunction),
    get_dict(CacheKey, CachedFunction, Result).

ok.

to_print(String, String) :- string(String),!.
to_print(Number, String) :- number(Number), number_string(Number, String),!.
to_print([First, Second], String) :-
    to_print(First, PrintableFirst),
    to_print(Second, PrintableSecond),
    string_concat(PrintableFirst, PrintableSecond, String).
to_print(Function, String) :-
    verifyKind(Function, "Function"),
    String = '<closure>'.

cache(FunctionName, Context, Arguments, Result, NewContext) :-
    get_cache_key(FunctionName, Context, Arguments, CacheKey),
    get_dict('__cache__', Context, Cache),
    (  get_dict(FunctionName, Cache, FunctionCache)
    -> ok
    ;  put_dict(FunctionName, Cache, cache{}, FunctionCache)),
    put_dict(CacheKey, FunctionCache, Result, NewFunctionCache),
    put_dict(FunctionName, Cache, NewFunctionCache, NewCache),
    put_dict('__cache__', Context, NewCache, NewContext).

evaluate(ValueToEvaluate, Context, EvaluatedValue, Context) :-
    (string(ValueToEvaluate); number(ValueToEvaluate)),
    EvaluatedValue = ValueToEvaluate,!.
evaluate(true, Context, true, Context).
evaluate(false, Context, false, Context).
evaluate(VarName, Context, VarValue, Context) :- str(VarName, VarValue),!.
evaluate(VarCode, Context, VarName, Context) :- 
    intvar(VarCode, VarName),!.
evaluate(VarCode, Context, VarValue, NewContext) :-
    var(VarCode, VarName),
    string_to_atom(VarName, VarNameAtom),
    get_dict(VarNameAtom, Context, VarValueObject),
    evaluate(VarValueObject, Context, VarValue, NewContext),!.
evaluate(VarName, Context, VarValue, Context) :-
    bool(VarName, VarValue),!.
evaluate([FirstValue, SecondValue], Context, [EvaluatedFirstValue, EvaluatedSecondValue], New_Context) :-
    evaluate(FirstValue, Context, EvaluatedFirstValue, Context1),
    put_dict(Context1, Context, Context2),
    evaluate(SecondValue, Context, EvaluatedSecondValue, Context3),
    put_dict(Context3, Context2, New_Context),!.
evaluate(FunctionCall, Context, Result, New_Context) :-
    functionCall(FunctionCall, Callee, Arguments),
    var(Callee, FunctionNameString),
    string_to_atom(FunctionNameString, FunctionName),
    get_dict(FunctionName, Context, Function),
    parse_args(Arguments, Context, EvaluatedArguments),
    (  cached(FunctionName, Context, EvaluatedArguments, Result)
    -> New_Context = Context
    ;  evaluate(FunctionName, Function, Context, EvaluatedArguments, Result, ContextCallOut),
       (  cache(FunctionName, ContextCallOut, EvaluatedArguments, Result, New_Context)
       -> ok
       ;  ok
       )
    ),!.
evaluate(BinaryOperation, Context, Result, New_Context) :-
    binary(BinaryOperation, Operation, LeftOperand, RightOperand),
    evaluate(LeftOperand, Context, EvaluatedLeftOperand, Context1),
    evaluate(RightOperand, Context1, EvaluatedRightOperand, New_Context),
    operate(EvaluatedLeftOperand, Operation, EvaluatedRightOperand, Result),!.
evaluate(IfStatement, Context, Result, New_Context) :-
    if(IfStatement, Condition, Then, Otherwise),
    evaluate(Condition, Context, ConditionResult, Context1),
    (  ConditionResult = true
    -> evaluate(Then, Context1, Result, New_Context)
    ;  evaluate(Otherwise, Context1, Result, New_Context)
    ),!.
evaluate(Function, Context, Result, Context) :-
    function(Function, _, _),
    Result = Function,!.
evaluate(LetExpression, Context, Result, NewContext) :-
    letExpression(LetExpression, VarName, Value, Next),
    evaluate(Value, Context, EvaluatedValue, Context1),
    string_to_atom(VarName, VarAtom),
    put_dict(VarAtom, Context1, EvaluatedValue, NewContext1),
    evaluate(Next, NewContext1, Result, NewContext),!.
evaluate(TupleExpression, Context, [FirstEvaluated, SecondEvaluated], Context) :-
    tuple(TupleExpression, [First, Second]),
    evaluate(First, Context, FirstEvaluated, _),
    evaluate(Second, Context, SecondEvaluated, _),!.
evaluate(FirstExpression, Context, First, Context) :-
    firstExpression(FirstExpression, Value),
    evaluate(Value, Context, Tuple, _),
    first(Tuple, First),!.
evaluate(SecondExpression, Context, Second, Context) :-
    secondExpression(SecondExpression, Value),
    evaluate(Value, Context, Tuple, _),
    second(Tuple, Second),!.    

evaluate(PrintExpression, Context, EvaluatedValue, Context) :-
    printExpr(PrintExpression, ValueToEvaluate),
    evaluate(ValueToEvaluate, Context, EvaluatedValue, _),
    to_print(EvaluatedValue, Printable),
    write(Printable),nl,!.
evaluate(FunctionName, Function, Context, Input, Result, NewContext) :-
    function(Function, Arguments, Body),
    length(Arguments, FuncionExpectedArgumentsNumber),
    length(Input, PassedArgumentsNumber),
    FuncionExpectedArgumentsNumber = PassedArgumentsNumber,
    bindParameters(Arguments, Input, Context, BoundParameters),
    put_dict(BoundParameters, Context, ContextWithParams),
    evaluate(Body, ContextWithParams, Result, Context1),
    get_cached_inputs(Context, Context1, FunctionName, NewContext),!.

get_cached_inputs(ContextIn, ContextOut, FunctionName, NewContext) :-
    get_dict('__cache__', ContextIn, InCache),
    get_dict(FunctionName, InCache, InFunctionCache),
    get_dict('__cache__', ContextOut, OutCache),
    get_dict(FunctionName, OutCache, OutFunctionCache),
    put_dict(OutFunctionCache, InFunctionCache, NewFunctionCache),
    put_dict(FunctionName, InCache, NewFunctionCache, NewCache),
    put_dict('__cache__', ContextIn, NewCache, NewContext).
get_cached_inputs(ContextIn, _, _, ContextIn).

run(ProgramAST) :- run(ProgramAST, context{'__cache__': cache{}}),!.
run(ProgramAST, Context) :-
    file(ProgramAST, Code),
    evaluate(Code, Context, _, _),!.
run(Exp, _) :-
    write('Syntax Error on line: '), write(Exp),!.

main :-
	current_prolog_flag(argv, Args),
	filenameFromArgs(Args, FileName),
    open(FileName, read, Stream),
    json_read_dict(Stream, ProgramAST),
    run(ProgramAST),nl.
