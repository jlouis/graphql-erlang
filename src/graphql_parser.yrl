Nonterminals
  Document

  Definitions Definition OperationDefinition FragmentDefinition TypeDefinition
  ObjectTypeDefinition InterfaceTypeDefinition UnionTypeDefinition
  ScalarTypeDefinition EnumTypeDefinition InputObjectTypeDefinition TypeExtensionDefinition
  FieldDefinitionList FieldDefinition ArgumentsDefinition
  
  ImplementsInterfaces 
  UnionMembers

  InputValueDefinitionList InputValueDefinition
  EnumValueDefinitionList EnumValueDefinition

  SelectionSet Selections Selection

  OperationType
  VariableDefinitions VariableDefinition
  Directives Directive
  Field Alias Arguments ArgumentList Argument
  FragmentSpread FragmentName InlineFragment
  VariableDefinitionList Variable DefaultValue
  
  Type TypeCondition NamedTypeList NamedType ListType NonNullType
  
  Name KeywordName
  Value EnumValue ListValue Values
  InputObjectValue InputObjectFields InputObjectField

  AnnotationList Annotation.

Terminals
  '{' '}' '(' ')' '[' ']' '!' ':' '@' '$' '=' '|' '...' '+'
  'query' 'mutation' 'subscription' 'fragment' 'on' 'null'
  'type' 'implements' 'interface' 'union' 'scalar' 'enum' 'input' 'extend'
  name int float bstring bool.

Rootsymbol Document.

Document -> Definitions : {document, '$1'}.

Definitions -> Definition : ['$1'].
Definitions -> Definition Definitions : ['$1'|'$2'].

Definition -> OperationDefinition : '$1'.
Definition -> FragmentDefinition : '$1'.
Definition -> TypeDefinition : '$1'.

OperationType -> 'query' : g_query('$1').
OperationType -> 'mutation' : g_mutation('$1').
OperationType -> 'subscription' : g_subscription('$1').

OperationDefinition -> SelectionSet :
    #op { selection_set = '$1' }.
OperationDefinition -> OperationType Name SelectionSet :
    #op { ty = '$1', id = '$2', selection_set = '$3' }.
OperationDefinition -> OperationType Name VariableDefinitions SelectionSet :
    #op { ty = '$1', id = '$2', vardefs = '$3', selection_set = '$4' }.
OperationDefinition -> OperationType Name Directives SelectionSet :
    #op { ty = '$1', id = '$2', directives = '$3', selection_set = '$4' }.
OperationDefinition -> OperationType Name VariableDefinitions Directives SelectionSet :
    #op { ty = '$1', id = '$2', vardefs = '$3', directives = '$4', selection_set = '$5' }.

FragmentDefinition -> 'fragment' FragmentName 'on' TypeCondition SelectionSet :
    #frag { id = '$2', ty = '$4', selection_set = '$5' }.
FragmentDefinition -> 'fragment' FragmentName 'on' TypeCondition Directives SelectionSet :
    #frag { id = '$2', ty = '$4', directives = '$5', selection_set = '$6' }.

TypeCondition -> NamedType : '$1'.

VariableDefinitions -> '(' VariableDefinitionList ')' : '$2'.

VariableDefinitionList -> VariableDefinition : ['$1'].
VariableDefinitionList -> VariableDefinition VariableDefinitionList : ['$1'|'$2'].

VariableDefinition -> Variable ':' Type :
    #vardef { id = '$1', ty = '$3', default = null }.
VariableDefinition -> Variable ':' Type DefaultValue :
    #vardef { id = '$1', ty = '$3', default = '$4' }.

Variable -> '$' Name : '$2'.

DefaultValue -> '=' Value : '$2'.

Type -> NamedType : '$1'.
Type -> ListType : '$1'.
Type -> NonNullType : '$1'.

NamedType -> Name : g_ty('$1').

ListType -> '[' Type ']' : {list, '$2'}.


NonNullType -> NamedType '!' : {non_null, '$1'}.
NonNullType -> ListType '!' : {non_null, '$1'}.

SelectionSet -> '{' Selections '}' : '$2'.

Selections -> Selection : ['$1'].
Selections -> Selection Selections : ['$1'|'$2'].

Selection -> Field : '$1'.
Selection -> FragmentSpread : '$1'.
Selection -> InlineFragment : '$1'.

FragmentSpread -> '...' FragmentName : 
    #frag_spread { id = '$2'}.
FragmentSpread -> '...' FragmentName Directives :
    #frag_spread { id = '$2', directives = '$3' }.

InlineFragment -> '...' SelectionSet :
    #frag { id = '...', selection_set = '$2' }.
InlineFragment -> '...' Directives SelectionSet :
    #frag { id = '...', directives = '$2', selection_set = '$3' }.
InlineFragment -> '...' 'on' TypeCondition SelectionSet :
    #frag { id = '...', ty = '$3', selection_set = '$4' }.
InlineFragment -> '...' 'on' TypeCondition Directives SelectionSet :
    #frag { id = '...', ty = '$3', directives = '$4', selection_set = '$5' }.

Field -> Name :
    #field { id = '$1' }.
Field -> Name Arguments :
    #field { id = '$1', args = '$2' }.
Field -> Name SelectionSet :
    #field { id = '$1', selection_set = '$2' }.
Field -> Name Arguments SelectionSet :
    #field { id = '$1', args = '$2', selection_set = '$3' }.
Field -> Name Directives :
    #field { id = '$1', directives = '$2' }.
Field -> Name Arguments Directives :
    #field { id = '$1', args = '$2', directives = '$3' }.
Field -> Name Directives SelectionSet :
    #field { id = '$1', directives = '$2', selection_set = '$3' }.
Field -> Name Arguments Directives SelectionSet :
    #field { id = '$1', args = '$2', directives = '$3', selection_set = '$4' }.

Field -> Alias Name :
      #field { alias = '$1', id = '$2' }.
Field -> Alias Name Arguments :
    #field { alias = '$1', id = '$2', args = '$3' }.
Field -> Alias Name SelectionSet :
    #field { alias = '$1', id = '$2', selection_set = '$3' }.
Field -> Alias Name Arguments SelectionSet :
    #field { alias = '$1', id = '$2', args = '$3', selection_set = '$4' }.
Field -> Alias Name Directives :
    #field { alias = '$1', id = '$2', directives = '$3' }.
Field -> Alias Name Arguments Directives :
    #field { alias = '$1', id = '$2', args = '$3', directives = '$4' }.
Field -> Alias Name Directives SelectionSet :
    #field { alias = '$1', id = '$2', directives = '$3', selection_set = '$4' }.
Field -> Alias Name Arguments Directives SelectionSet :
    #field {
        alias = '$1',
        id = '$2',
        args = '$3',
        directives = '$4',
        selection_set = '$5'
    }.

Alias -> Name ':' : '$1'.

Arguments -> '(' ArgumentList ')' : '$2'.

ArgumentList -> Argument : ['$1'].
ArgumentList -> Argument ArgumentList : ['$1'|'$2'].

Argument -> Name ':' Value : {'$1', '$3'}.

Directives -> Directive : ['$1'].
Directives -> Directive Directives : ['$1'|'$2'].

Directive -> '@' Name :
    #directive { id = '$2' }.
Directive -> '@' Name Arguments :
    #directive { id = '$2', args = '$3' }.

KeywordName -> 'query' : keyword('$1').
KeywordName -> 'mutation' : keyword('$1').
KeywordName -> 'fragment' : keyword('$1').
KeywordName -> 'type' : keyword('$1').
KeywordName -> 'implements' : keyword('$1').
KeywordName -> 'interface' : keyword('$1').
KeywordName -> 'union' : keyword('$1').
KeywordName -> 'scalar' : keyword('$1').
KeywordName -> 'enum' : keyword('$1').
KeywordName -> 'input' : keyword('$1').
KeywordName -> 'extend' : keyword('$1').
KeywordName -> 'null' : keyword('$1').

FragmentName -> name : '$1'.
FragmentName -> KeywordName : '$1'.

Name -> name : '$1'.
Name -> KeywordName : '$1'.
Name -> 'on' : '$1'.

Value -> Variable : g_var('$1').
Value -> int : g_integer('$1').
Value -> float : g_float('$1').
Value -> bstring : g_string('$1').
Value -> bool : g_bool('$1').
Value -> EnumValue : {enum, '$1'}.
Value -> ListValue : g_list('$1').
Value -> InputObjectValue : g_input_object('$1').

EnumValue -> Name : g_enum('$1').

ListValue -> '[' ']' : [].
ListValue -> '[' Values ']' : '$2'.

Values -> Value : ['$1'].
Values -> Value Values : ['$1'|'$2'].

InputObjectValue -> '{' '}' : [].
InputObjectValue -> '{' InputObjectFields '}' : '$2'.

InputObjectFields -> InputObjectField : ['$1'].
InputObjectFields -> InputObjectField InputObjectFields : ['$1'|'$2'].

InputObjectField -> Name ':' Value : {'$1', '$3'}.

TypeDefinition -> ObjectTypeDefinition : '$1'.
TypeDefinition -> InterfaceTypeDefinition : '$1'.
TypeDefinition -> UnionTypeDefinition : '$1'.
TypeDefinition -> ScalarTypeDefinition : '$1'.
TypeDefinition -> EnumTypeDefinition : '$1'.
TypeDefinition -> InputObjectTypeDefinition : '$1'.
TypeDefinition -> TypeExtensionDefinition : '$1'.

AnnotationList -> Annotation : ['$1'].
AnnotationList -> Annotation AnnotationList : ['$1'|'$2'].

Annotation -> '+' Name : {annotation, '$2', []}.
Annotation -> '+' Name Arguments: {annotation, '$2', '$3'}.

ObjectTypeDefinition -> AnnotationList 'type' Name '{' FieldDefinitionList '}' : {object_type, '$3', [], '$5', '$1'}.
ObjectTypeDefinition -> 'type' Name '{' FieldDefinitionList '}' :
  {object_type, '$2', [], '$4', []}.
ObjectTypeDefinition -> AnnotationList 'type' Name ImplementsInterfaces '{' FieldDefinitionList '}' :
  {object_type, '$3', '$4', '$6', '$1'}.
ObjectTypeDefinition -> 'type' Name ImplementsInterfaces '{' FieldDefinitionList '}' :
  {object_type, '$2', '$3', '$5', []}.

ImplementsInterfaces -> 'implements' NamedTypeList : '$2'.

NamedTypeList -> NamedType : ['$1'].
NamedTypeList -> NamedType NamedTypeList : ['$1'|'$2'].

FieldDefinitionList -> FieldDefinition : ['$1'].
FieldDefinitionList -> FieldDefinition FieldDefinitionList : ['$1'|'$2'].

FieldDefinition -> Name ':' Type : {field_def, '$1', [], '$3', [], []}.
FieldDefinition -> Name ':' Type Directives : {field_def, '$1', [], '$3', '$4', []}.
FieldDefinition -> Name ArgumentsDefinition ':' Type : {field_def, '$1', '$2', '$4', [], []}.
FieldDefinition -> Name ArgumentsDefinition ':' Type Directives : {field_def, '$1', '$2', '$4', '$5', []}.

FieldDefinition -> AnnotationList Name ':' Type : {field_def, '$2', [], '$4', [], '$1'}.
FieldDefinition -> AnnotationList Name ':' Type Directives : {field_def, '$2', [], '$5', '$5', '$1'}.
FieldDefinition -> AnnotationList Name ArgumentsDefinition ':' Type : {field_def, '$2', '$3', '$5', [], '$1'}.
FieldDefinition -> AnnotationList Name ArgumentsDefinition ':' Type Directives : {field_def, '$2', '$3', '$5', '$6', '$1'}.

ArgumentsDefinition -> '(' InputValueDefinitionList ')' : '$2'.

InputValueDefinitionList -> InputValueDefinition : ['$1'].
InputValueDefinitionList -> InputValueDefinition InputValueDefinitionList : ['$1'|'$2'].


InputValueDefinition -> AnnotationList Name ':' Type :
                            {'$2', '$4', '$1'}.
InputValueDefinition -> Name ':' Type :
                            {'$1', '$3', []}.
InputValueDefinition -> AnnotationList Name ':' Type DefaultValue :
                            {'$2', '$4', '$5', '$1'}.
InputValueDefinition -> Name ':' Type DefaultValue :
                            {'$1', '$3', '$4', []}.

InterfaceTypeDefinition -> 'interface' Name '{' FieldDefinitionList '}' :
    {interface, '$1', '$4'}.

UnionTypeDefinition -> 'union' Name '=' UnionMembers :
    {union, '$2', '$4'}.

UnionMembers -> NamedType : ['$1'].
UnionMembers -> NamedType '|' UnionMembers : ['$1'|'$3'].

ScalarTypeDefinition -> AnnotationList 'scalar' Name : {scalar, '$3', '$1'}.
ScalarTypeDefinition -> 'scalar' Name : {scalar, '$2', []}.

EnumTypeDefinition -> 'enum' Name '{' EnumValueDefinitionList '}':
    {enum, '$2', '$4'}.

EnumValueDefinitionList -> EnumValueDefinition : ['$1'].
EnumValueDefinitionList -> EnumValueDefinition EnumValueDefinitionList : ['$1'|'$2'].

EnumValueDefinition -> EnumValue : '$1'.

InputObjectTypeDefinition -> AnnotationList 'input' Name '{' InputValueDefinitionList '}' : {input, '$3', '$5', '$1'}.
InputObjectTypeDefinition -> 'input' Name '{' InputValueDefinitionList '}' :
    {input, '$2', '$4', []}.

TypeExtensionDefinition -> 'extend' ObjectTypeDefinition :
    {extend, '$2'}.

Erlang code.

-include("graphql_internal.hrl").

g_query({query, _L} = Q) -> Q.
g_mutation({mutation, _L} = Mut) -> Mut.
g_subscription({subscription, _L} = Sub) -> Sub.

g_ty({name, _, <<"String">>}) -> {scalar, string};
g_ty({name, _, <<"string">>}) -> {scalar, string};
g_ty({name, _, <<"Int">>}) -> {scalar, int};
g_ty({name, _, <<"int">>}) -> {scalar, int};
g_ty({name, _, <<"float">>}) -> {scalar, float};
g_ty({name, _, <<"Float">>}) -> {scalar, float};
g_ty({name, _, <<"id">>}) -> {scalar, id};
g_ty({name, _, <<"Id">>}) -> {scalar, id};
g_ty({name, _, <<"ID">>}) -> {scalar, id};
g_ty({name, _, _} = N) -> N.

g_enum({name, _Line, N}) -> N.

g_var({name, _, _} = N) -> {var, N}.
g_integer({int, _, I}) -> I.
g_float({float, _, F}) -> F.
g_string({bstring, _, S}) -> S.
g_bool({bool, _, B}) -> B.

g_list(L) when is_list(L) -> L.
g_input_object(KVPairs) -> maps:from_list(KVPairs).

%% Convert keywords into binaries if they don't occur in the KW-position
keyword({A, Line}) when is_atom(A) ->
    {name, Line, atom_to_binary(A, utf8)}.
