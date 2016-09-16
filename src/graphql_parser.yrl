Nonterminals
  Document
  Definitions Definition OperationDefinition FragmentDefinition TypeDefinition
  ObjectTypeDefinition InterfaceTypeDefinition UnionTypeDefinition
  ScalarTypeDefinition EnumTypeDefinition InputObjectTypeDefinition TypeExtensionDefinition
  FieldDefinitionList FieldDefinition ImplementsInterfaces ArgumentsDefinition
  InputValueDefinitionList InputValueDefinition UnionMembers
  EnumValueDefinitionList EnumValueDefinition
  SelectionSet Selections Selection
  OperationType Name NameWithoutOn VariableDefinitions VariableDefinition Directives Directive
  Field Alias Arguments ArgumentList Argument
  FragmentSpread FragmentName InlineFragment
  VariableDefinitionList Variable DefaultValue
  Type TypeCondition NamedTypeList NamedType ListType NonNullType
  Value EnumValue ListValue Values ObjectValue ObjectFields ObjectField.

Terminals
  '{' '}' '(' ')' '[' ']' '!' ':' '@' '$' '=' '|' '...'
  'query' 'mutation' 'subscription' 'fragment' 'on' 'null'
  'type' 'implements' 'interface' 'union' 'scalar' 'enum' 'input' 'extend'
  name int float string bool.

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
    #op { ty = '$1', id = g_name('$2'), selection_set = '$3' }.
OperationDefinition -> OperationType Name VariableDefinitions SelectionSet :
    #op { ty = '$1', id = g_name('$2'), vardefs = '$3', selection_set = '$4' }.
OperationDefinition -> OperationType Name Directives SelectionSet :
    #op { ty = '$1', id = g_name('$2'), directives = '$3', selection_set = '$4' }.
OperationDefinition -> OperationType Name VariableDefinitions Directives SelectionSet :
    #op { ty = '$1', id = g_name('$2'), vardefs = '$3', directives = '$4', selection_set = '$5' }.

FragmentDefinition -> 'fragment' FragmentName 'on' TypeCondition SelectionSet :
    #frag { id = g_name('$2'), ty = g_ty('$4'), selection_set = '$5' }.
FragmentDefinition -> 'fragment' FragmentName 'on' TypeCondition Directives SelectionSet :
    #frag { id = g_name('$2'), ty = g_ty('$4'), directives = '$5', selection_set = '$6' }.

TypeCondition -> NamedType : '$1'.

VariableDefinitions -> '(' VariableDefinitionList ')' : '$2'.

VariableDefinitionList -> VariableDefinition : ['$1'].
VariableDefinitionList -> VariableDefinition VariableDefinitionList : ['$1'|'$2'].

VariableDefinition -> Variable ':' Type :
    #vardef { id = g_name('$1'), ty = '$3', default = undefined }.
VariableDefinition -> Variable ':' Type DefaultValue :
    #vardef { id = g_name('$1'), ty = '$3', default = '$4' }.

Variable -> '$' Name : {var, g_name('$2')}.

DefaultValue -> '=' Value : '$2'.

Type -> NamedType : {ty, '$1'}.
Type -> ListType : {ty, '$1'}.
Type -> NonNullType : {ty, '$1'}.

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
    #frag_spread { id = g_name('$2')}.
FragmentSpread -> '...' FragmentName Directives :
    #frag_spread { id = g_name('$2'), directives = '$3' }.

InlineFragment -> '...' SelectionSet :
    #frag { id = '...', selection_set = '$2' }.
InlineFragment -> '...' Directives SelectionSet :
    #frag { id = '...', directives = '$2', selection_set = '$3' }.
InlineFragment -> '...' 'on' TypeCondition SelectionSet :
    #frag { id = '...', ty = '$3', selection_set = '$4' }.
InlineFragment -> '...' 'on' TypeCondition Directives SelectionSet :
    #frag { id = '...', ty = '$3', directives = '$4', selection_set = '$5' }.

FragmentName -> NameWithoutOn : '$1'.

Field -> Name :
    #field { id = g_name('$1') }.
Field -> Name Arguments :
    #field { id = g_name('$1'), args = '$2' }.
Field -> Name SelectionSet :
    #field { id = g_name('$1'), selection_set = '$2' }.
Field -> Name Arguments SelectionSet :
    #field { id = g_name('$1'), args = '$2', selection_set = '$3' }.
Field -> Name Directives :
    #field { id = g_name('$1'), directives = '$2' }.
Field -> Name Arguments Directives :
    #field { id = g_name('$1'), args = '$2', directives = '$3' }.
Field -> Name Directives SelectionSet :
    #field { id = g_name('$1'), directives = '$2', selection_set = '$3' }.
Field -> Name Arguments Directives SelectionSet :
    #field { id = g_name('$1'), args = '$2', directives = '$3', selection_set = '$4' }.

Field -> Alias Name :
      #field { alias = g_name('$1'), id = g_name('$2') }.
Field -> Alias Name Arguments :
    #field { alias = g_name('$1'), id = g_name('$2'), args = '$3' }.
Field -> Alias Name SelectionSet :
    #field { alias = g_name('$1'), id = g_name('$2'), selection_set = '$3' }.
Field -> Alias Name Arguments SelectionSet :
    #field { alias = g_name('$1'), id = g_name('$2'), args = '$3', selection_set = '$4' }.
Field -> Alias Name Directives :
    #field { alias = g_name('$1'), id = g_name('$2'), directives = '$3' }.
Field -> Alias Name Arguments Directives :
    #field { alias = g_name('$1'), id = g_name('$2'), args = '$3', directives = '$4' }.
Field -> Alias Name Directives SelectionSet :
    #field { alias = g_name('$1'), id = g_name('$2'), directives = '$3', selection_set = '$4' }.
Field -> Alias Name Arguments Directives SelectionSet :
    #field {
        alias = g_name('$1'),
        id = g_name('$2'),
        args = '$3',
        directives = '$4',
        selection_set = '$5'
    }.

Alias -> Name ':' : '$1'.

Arguments -> '(' ArgumentList ')' : '$2'.

ArgumentList -> Argument : ['$1'].
ArgumentList -> Argument ArgumentList : ['$1'|'$2'].

Argument -> Name ':' Value : {g_name('$1'), '$3'}.

Directives -> Directive : ['$1'].
Directives -> Directive Directives : ['$1'|'$2'].

Directive -> '@' Name :
    #directive { id = g_name('$2') }.
Directive -> '@' Name Arguments :
    #directive { id = g_name('$2'), args = '$3' }.

NameWithoutOn -> name : '$1'.
NameWithoutOn -> 'query' : '$1'.
NameWithoutOn -> 'mutation' : '$1'.
NameWithoutOn -> 'fragment' : '$1'.
NameWithoutOn -> 'type' : '$1'.
NameWithoutOn -> 'implements' : '$1'.
NameWithoutOn -> 'interface' : '$1'.
NameWithoutOn -> 'union' : '$1'.
NameWithoutOn -> 'scalar' : '$1'.
NameWithoutOn -> 'enum' : '$1'.
NameWithoutOn -> 'input' : '$1'.
NameWithoutOn -> 'extend' : '$1'.
NameWithoutOn -> 'null' : '$1'.

Name -> NameWithoutOn : '$1'.
Name -> 'on' : '$1'.

Value -> Variable : '$1'.
Value -> int : g_integer('$1').
Value -> float : g_float('$1').
Value -> string : g_string('$1').
Value -> bool : g_bool('$1').
Value -> EnumValue : {enum, '$1'}.
Value -> ListValue : {list, '$1'}.
Value -> ObjectValue : {object, '$1'}.

EnumValue -> Name : '$1'.

ListValue -> '[' ']' : [].
ListValue -> '[' Values ']' : '$2'.

Values -> Value : ['$1'].
Values -> Value Values : ['$1'|'$2'].

ObjectValue -> '{' '}' : [].
ObjectValue -> '{' ObjectFields '}' : '$2'.

ObjectFields -> ObjectField : ['$1'].
ObjectFields -> ObjectField ObjectFields : ['$1'|'$2'].

ObjectField -> Name ':' Value : {g_name('$1'), '$3'}.

TypeDefinition -> ObjectTypeDefinition : '$1'.
TypeDefinition -> InterfaceTypeDefinition : '$1'.
TypeDefinition -> UnionTypeDefinition : '$1'.
TypeDefinition -> ScalarTypeDefinition : '$1'.
TypeDefinition -> EnumTypeDefinition : '$1'.
TypeDefinition -> InputObjectTypeDefinition : '$1'.
TypeDefinition -> TypeExtensionDefinition : '$1'.

ObjectTypeDefinition -> 'type' Name '{' FieldDefinitionList '}' :
  {object_type, g_name('$2'), [], '$4'}.
ObjectTypeDefinition -> 'type' Name ImplementsInterfaces '{' FieldDefinitionList '}' :
  {object_type, g_name('$3'), '$3', '$5'}.

ImplementsInterfaces -> 'implements' NamedTypeList : '$2'.

NamedTypeList -> NamedType : ['$1'].
NamedTypeList -> NamedType NamedTypeList : ['$1'|'$2'].

FieldDefinitionList -> FieldDefinition : ['$1'].
FieldDefinitionList -> FieldDefinition FieldDefinitionList : ['$1'|'$2'].

FieldDefinition -> Name ':' Type : {field_def, g_name('$1'), [], '$3'}.
FieldDefinition -> Name ArgumentsDefinition ':' Type : {field, g_name('$1'), '$2', '$4'}.

ArgumentsDefinition -> '(' InputValueDefinitionList ')' : '$2'.

InputValueDefinitionList -> InputValueDefinition : ['$1'].
InputValueDefinitionList -> InputValueDefinition InputValueDefinitionList : ['$1'|'$2'].

InputValueDefinition -> Name ':' Type : {g_name('$1'), '$2'}.
InputValueDefinition -> Name ':' Type DefaultValue : {g_name('$1'), '$2', '$4'}.

InterfaceTypeDefinition -> 'interface' Name '{' FieldDefinitionList '}' :
    {interface, g_name('$1'), '$4'}.

UnionTypeDefinition -> 'union' Name '=' UnionMembers :
    {union, g_name('$2'), '$4'}.

UnionMembers -> NamedType : ['$1'].
UnionMembers -> NamedType '|' UnionMembers : ['$1'|'$3'].

ScalarTypeDefinition -> 'scalar' Name : {scalar, g_name('$1')}.

EnumTypeDefinition -> 'enum' Name '{' EnumValueDefinitionList '}':
    {enum, g_name('$2'), '$4'}.

EnumValueDefinitionList -> EnumValueDefinition : ['$1'].
EnumValueDefinitionList -> EnumValueDefinition EnumValueDefinitionList : ['$1'|'$2'].

EnumValueDefinition -> EnumValue : '$1'.

InputObjectTypeDefinition -> 'input' Name '{' InputValueDefinitionList '}' :
    {input, g_name('$2'), '$4'}.

TypeExtensionDefinition -> 'extend' ObjectTypeDefinition :
    {extend, '$2'}.

Erlang code.

-include("graphql_internal.hrl").

g_query({query, _L} = Q) -> Q.
g_mutation({mutation, _L} = Mut) -> Mut.
g_subscription({subscription, _L} = Sub) -> Sub.

g_name({name, _ID, _Line} = N) -> N;
g_name({var, N}) -> g_name(N).

g_ty({name, <<"String">>, _}) -> {scalar, string};
g_ty({name, <<"string">>, _}) -> {scalar, string};
g_ty({name, <<"Int">>, _}) -> {scalar, int};
g_ty({name, <<"int">>, _}) -> {scalar, int};
g_ty({name, <<"float">>, _}) -> {scalar, float};
g_ty({name, <<"Float">>, _}) -> {scalar, float};
g_ty({name, <<"id">>, _}) -> {scalar, id};
g_ty({name, <<"Id">>, _}) -> {scalar, id};
g_ty({name, <<"ID">>, _}) -> {scalar, id};
g_ty({scalar, Ty}) -> {scalar, Ty};
g_ty(N) -> g_name(N).

g_integer({int, I, _}) -> I.

g_float({float, F, _}) -> F.

g_string({string, S, _}) -> S.

g_bool({bool, B, _}) -> B.
