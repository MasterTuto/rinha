:- use_module(library(http/json)).

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
    evaluate(Value, Context, EvaluatedValue),
    string_to_atom(Name, NameAtom),
    put_dict(NameAtom, ParametersObject1, EvaluatedValue, ParametersObject).

different(X, X, false) :- !.
different(_, _, true).

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

int(IntCode, Value) :- 
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

first([First,_], First).

second([_, Second], Second).

operate(LeftOperand, "Add", RightOperand, Result) :-
    Result is LeftOperand + RightOperand.
operate(LeftOperand, "Sub", RightOperand, Result) :-
    Result is LeftOperand - RightOperand.
operate(LeftOperand, "Mul", RightOperand, Result) :-
    Result is LeftOperand * RightOperand.
operate(LeftOperand, "Div", RightOperand, Result) :-
    Result is LeftOperand / RightOperand.
operate(LeftOperand, "Rem", RightOperand, Result) :-
    Result is LeftOperand rem RightOperand.
operate(LeftOperand, "Eq", LeftOperand, true).
operate(_, "Eq", _, false).
operate(LeftOperand, "Neq", RightOperand, Result) :-
    different(LeftOperand, RightOperand, Result).
operate(LeftOperand, "Lt", RightOperand, Result) :-
    (
       LeftOperand < RightOperand
    -> Result = true 
    ;  Result = false).
operate(LeftOperand, "Gt", RightOperand, Result) :-
    (
       LeftOperand > RightOperand
    -> Result = true 
    ;  Result = false).
operate(LeftOperand, "Lte", RightOperand, Result) :-
    (
       LeftOperand =< RightOperand
    -> Result = true 
    ;  Result = false).
operate(LeftOperand, "Gte", RightOperand, Result) :-
    (
        LeftOperand >= RightOperand
    -> Result = true 
    ;  Result = false).
operate(true, "And", true, true).
operate(true, "And", false, false).
operate(false, "And", true, false).
operate(false, "And", false, false).
operate(true, "Or", true, true).
operate(true, "Or", false, true).
operate(false, "Or", true, true).
operate(false, "Or", false, false).

cached(_, cached{}, _, _) :- fail,!.
cached(FunctionName, Context, Arguments, Result) :-
    get_dict(FunctionName, Context, Arguments, R)

evaluate(ValueToEvaluate, _, EvaluatedValue) :-
    (string(ValueToEvaluate); number(ValueToEvaluate)),
    EvaluatedValue = ValueToEvaluate,!.
evaluate(true, _, true).
evaluate(false, _, false).
evaluate(VarName, _, VarValue) :- str(VarName, VarValue),!.
evaluate(VarName, _, VarValue) :- 
    int(VarName, VarValue),!.
evaluate(VarCode, Context, VarValue) :-
    var(VarCode, VarName),
    string_to_atom(VarName, VarNameAtom),
    get_dict(VarNameAtom, Context, VarValueObject),
    evaluate(VarValueObject, Context, VarValue).
evaluate(VarName, _, VarValue) :-
    bool(VarName, VarValue),!.
evaluate([FirstValue, SecondValue], Context, [EvaluatedFirstValue, EvaluatedSecondValue]) :-
    evaluate(FirstValue, Context, EvaluatedFirstValue),
    evaluate(SecondValue, Context, EvaluatedSecondValue),!.
evaluate(FunctionCall, Context, Result) :-
    functionCall(FunctionCall, Callee, Arguments),
    var(Callee, FunctionNameString),
    string_to_atom(FunctionNameString, FunctionName),
    get_dict(FunctionName, Context, Function),
    (
        cached(FunctionName, Context, Arguments, Result);
        evaluate(Function, Context, Arguments, Result)
    ).
evaluate(BinaryOperation, Context, Result) :-
    binary(BinaryOperation, Operation, LeftOperand, RightOperand),
    evaluate(LeftOperand, Context, EvaluatedLeftOperand),
    evaluate(RightOperand, Context, EvaluatedRightOperand),
    operate(EvaluatedLeftOperand, Operation, EvaluatedRightOperand, Result).
evaluate(IfStatement, Context, Result) :-
    if(IfStatement, Condition, Then, Otherwise),
    evaluate(Condition, Context, ConditionResult),
    (  ConditionResult = true
    -> evaluate(Then, Context, Result)
    ;  evaluate(Otherwise, Context, Result)
    ).
evaluate(Function, _, Result) :-
    function(Function, _, _),
    Result = Function.
evaluate(Function, Context, Input, Result) :-
    function(Function, Arguments, Body),
    length(Arguments, FuncionExpectedArgumentsNumber),
    length(Input, PassedArgumentsNumber),
    FuncionExpectedArgumentsNumber = PassedArgumentsNumber,
    bindParameters(Arguments, Input, Context, BoundParameters),
    put_dict(BoundParameters, Context, NewContext),
    evaluate(Body, NewContext, Result).


run(ProgramAST) :- run(ProgramAST, context{}),!.
run(ProgramAST, Context) :-
    file(ProgramAST, Code),
    run(Code, Context),!.
run(LetExpression, Context) :-
    letExpression(LetExpression, VarName, Value, Next),
    evaluate(Value, Context, EvaluatedValue),
    string_to_atom(VarName, VarAtom),
    put_dict(VarAtom, Context, EvaluatedValue, NewContext),
    run(Next, NewContext),!.
run(PrintExpression, Context) :-
    printExpr(PrintExpression, ValueToEvaluate),
    evaluate(ValueToEvaluate, Context, EvaluatedValue),
    write(EvaluatedValue),!.
run(Exp, _) :-
    write('Syntax Error on line: '), write(Exp),!.

main :-
	current_prolog_flag(argv, Args),
	filenameFromArgs(Args, FileName),
    open(FileName, read, Stream),
    json_read_dict(Stream, ProgramAST),
    run(ProgramAST),nl.
