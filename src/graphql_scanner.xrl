% GraphQL Lexer
%
% See the spec reference
% http://facebook.github.io/graphql/#sec-Appendix-Grammar-Summary
% The relevant version is also copied into this repo

Definitions.

% Ignored tokens
WhiteSpace          = [\x{0009}\x{000B}\x{000C}\x{0020}\x{00A0}]
_LineTerminator     = \x{000A}\x{000D}\x{2028}\x{2029}
LineTerminator      = [{_LineTerminator}]
Comment             = #[^{_LineTerminator}]*
Comma               = ,
Ignored             = {WhiteSpace}|{LineTerminator}|{Comment}|{Comma}

% Lexical tokens
Punctuator          = [!$():=@\[\]{|}]|\.\.\.
Name                = [_A-Za-z][_0-9A-Za-z]*

% Int Value
Digit               = [0-9]
NonZeroDigit        = [1-9]
NegativeSign        = -
IntegerPart         = {NegativeSign}?(0|{NonZeroDigit}{Digit}*)
IntValue            = {IntegerPart}

% Float Value
FractionalPart      = \.{Digit}+
Sign                = [+\-]
ExponentIndicator   = [eE]
ExponentPart        = {ExponentIndicator}{Sign}?{Digit}+
FloatValue          = {IntegerPart}{FractionalPart}|{IntegerPart}{ExponentPart}|{IntegerPart}{FractionalPart}{ExponentPart}

% String Value
HexDigit            = [0-9A-Fa-f]
EscapedUnicode      = u{HexDigit}{HexDigit}{HexDigit}{HexDigit}
EscapedCharacter    = ["\\\/bfnrt]
StringCharacter     = ([^\"{_LineTerminator}]|\\{EscapedUnicode}|\\{EscapedCharacter})
StringValue         = "{StringCharacter}*"

Rules.

{Ignored}		: skip_token.
{Punctuator}	: {token, {list_to_atom(TokenChars), TokenLine}}.
{IntValue}		: {token, {int, TokenLine, list_to_integer(TokenChars)}}.
{FloatValue}	: {token, {float, TokenLine, list_to_float(TokenChars)}}.
{StringValue}	: {token, {string, TokenLine, iolist_to_binary(unquote(TokenChars))}}.
{Name}		: {token, identifier(TokenChars, TokenLine)}.

Erlang code.

%% Quell a warning in the leex header file for yyrev/2 which our current generated code
%% will never call.
-dialyzer({nowarn_function, yyrev/2}).

identifier("true", TokenLine) -> {bool, TokenLine, true};
identifier("false", TokenLine) -> {bool, TokenLine, false};
identifier("query", TokenLine) -> {query, TokenLine};
identifier("mutation", TokenLine) -> {mutation, TokenLine};
identifier("subscription", TokenLine) -> {subscription, TokenLine};
identifier("fragment", TokenLine) -> {fragment, TokenLine};
identifier("on", TokenLine) -> {on, TokenLine};
identifier("null", TokenLine) -> {null, TokenLine};
identifier(ID, TokenLine) -> {name, TokenLine, iolist_to_binary(ID)}.

unquote(Str) ->
    string:strip(Str, both, $").
