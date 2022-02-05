Nonterminals
  Document

  Definitions
    Definition

  OperationDefinition
  FragmentDefinition

  TypeSystemDefinition
    DirectiveDefinition
      DirectiveLocations
        DirectiveLocation

    SchemaDefinition
      RootOperationList
        RootOperationTypeDefinition
    TypeDefinition
      ObjectTypeDefinition
        ImplementsInterfaces

      InterfaceTypeDefinition
      UnionTypeDefinition
        UnionMemberTypes
      ScalarTypeDefinition
      EnumTypeDefinition
        EnumValueDefinitionList
        EnumValuesDefinition
        EnumValueDefinition
      InputObjectTypeDefinition
        InputFieldsDefinition
        InputValueDefinitionList
        InputValueDefinition

  FieldDefinitionList FieldDefinition FieldsDefinition
  ArgumentsDefinition

  SelectionSet Selections Selection
  Description 
  OperationType
  VariableDefinitions VariableDefinition
  Directives Directive
  Field Alias
  Arguments ArgumentList Argument
  FragmentSpread FragmentName InlineFragment
  VariableDefinitionList Variable DefaultValue
  
  Type TypeCondition NamedType ListType NonNullType
  
  Name KeywordName
   
  Values Value EnumValue ListValue
  InputObjectValue InputObjectFields InputObjectField.

Terminals
  '{' '}' '(' ')' '[' ']' '!' ':' '&' '@' '$' '=' '|' '...'
  'query' 'mutation' 'subscription' 'fragment' 'on' 'null'
  'type' 'implements' 'interface' 'union' 'scalar' 'enum' 'input' 'extend'
  'schema'
  'directive'

  name int float bstring bool.

Rootsymbol Document.

Document -> Definitions
  : #document { definitions = '$1' }.

Definitions -> Definition : ['$1'].
Definitions -> Definition Definitions : ['$1'|'$2'].

Definition -> OperationDefinition : '$1'.
Definition -> FragmentDefinition : '$1'.
Definition -> TypeSystemDefinition : '$1'.

OperationType -> 'query' : g_query('$1').
OperationType -> 'mutation' : g_mutation('$1').
OperationType -> 'subscription' : g_subscription('$1').

OperationDefinition -> SelectionSet :
    #op { selection_set = '$1' }.
OperationDefinition -> OperationType SelectionSet :
    #op { ty = '$1', selection_set = '$2' }.
OperationDefinition -> OperationType Name SelectionSet :
    #op { ty = '$1', id = '$2', selection_set = '$3' }.
OperationDefinition -> OperationType VariableDefinitions SelectionSet :
    #op { ty = '$1', vardefs = '$2', selection_set = '$3' }.
OperationDefinition -> OperationType VariableDefinitions Directives SelectionSet :
    #op { ty = '$1', vardefs = '$2', directives = '$3', selection_set = '$4' }.
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
    #vardef { id = '$1', ty = '$3' }.
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
    #frag_spread { id = '$2' }.
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
KeywordName -> 'subscription' : keyword('$1').
KeywordName -> 'fragment' : keyword('$1').
KeywordName -> 'type' : keyword('$1').
KeywordName -> 'implements' : keyword('$1').
KeywordName -> 'interface' : keyword('$1').
KeywordName -> 'union' : keyword('$1').
KeywordName -> 'scalar' : keyword('$1').
KeywordName -> 'enum' : keyword('$1').
KeywordName -> 'input' : keyword('$1').
KeywordName -> 'extend' : keyword('$1').

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
Value -> null : g_null('$1').

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

TypeSystemDefinition -> SchemaDefinition : '$1'.
TypeSystemDefinition -> TypeDefinition : '$1'.
TypeSystemDefinition -> DirectiveDefinition : '$1'.


SchemaDefinition -> 'schema' '{' RootOperationList '}'
    : #p_schema_definition { defs = '$3' }.
SchemaDefinition -> 'schema' Directives '{' RootOperationTypeDefinition '}'
    : #p_schema_definition { directives = '$2', defs = '$4' }.
    
RootOperationList -> RootOperationTypeDefinition
    : ['$1'].
RootOperationList -> RootOperationTypeDefinition RootOperationList
    : ['$1'|'$2'].
    
RootOperationTypeDefinition -> OperationType ':' Type
    : #p_root_operation { op_type = '$1', type = '$3' }. 

Description -> bstring : g_string('$1').

TypeDefinition -> ScalarTypeDefinition : '$1'.
TypeDefinition -> ObjectTypeDefinition : '$1'.
TypeDefinition -> InterfaceTypeDefinition : '$1'.
TypeDefinition -> UnionTypeDefinition : '$1'.
TypeDefinition -> EnumTypeDefinition : '$1'.
TypeDefinition -> InputObjectTypeDefinition : '$1'.

DirectiveDefinition -> 'directive' '@' Name 'on' DirectiveLocations
  : #p_directive{ id = '$3', locations = '$5' }.
DirectiveDefinition -> Description 'directive' '@' Name 'on' DirectiveLocations
  : #p_directive{ description = '$1', id = '$4', locations = '$6' }.
DirectiveDefinition -> 'directive' '@' Name ArgumentsDefinition 'on' DirectiveLocations
  : #p_directive{ id = '$3', args = '$4', locations = '$6' }.
DirectiveDefinition -> Description 'directive' '@' Name ArgumentsDefinition 'on' DirectiveLocations
  : #p_directive{ description = '$1', id = '$4', args = '$5', locations = '$7' }.

DirectiveLocations -> DirectiveLocation
  : ['$1'].
DirectiveLocations -> '|' DirectiveLocation
  : ['$2'].
DirectiveLocations -> DirectiveLocations '|' DirectiveLocation
  : ['$3'|'$1'].

DirectiveLocation -> name
  : g_directive_location('$1').

ScalarTypeDefinition -> 'scalar' Name
  : #p_scalar { id = '$2' }.
ScalarTypeDefinition -> Description 'scalar' Name
  : #p_scalar { description = '$1', id = '$3'}.
ScalarTypeDefinition -> Description 'scalar' Name Directives
  : #p_scalar { description = '$1', id = '$3', directives = '$4'}.
ScalarTypeDefinition -> 'scalar' Name Directives
  : #p_scalar { id = '$2', directives = '$3' }.

ObjectTypeDefinition -> 'type' Name FieldsDefinition
  : #p_object { id = '$2', fields = '$3' }.
ObjectTypeDefinition -> 'type' Name Directives FieldsDefinition
  : #p_object { id = '$2', directives = '$3', fields = '$4' }.
ObjectTypeDefinition -> 'type' Name ImplementsInterfaces FieldsDefinition
  : #p_object { id = '$2', interfaces = '$3', fields = '$4' }.
ObjectTypeDefinition -> 'type' Name ImplementsInterfaces Directives FieldsDefinition
  : #p_object { id = '$2', interfaces = '$3', directives = '$4', fields = '$5' }.
ObjectTypeDefinition -> Description 'type' Name FieldsDefinition
  : #p_object { description = '$1', id = '$3', fields = '$4' }.
ObjectTypeDefinition -> Description 'type' Name Directives FieldsDefinition
  : #p_object { description = '$1', id = '$3', directives = '$4', fields = '$5' }.
ObjectTypeDefinition -> Description 'type' Name ImplementsInterfaces FieldsDefinition
  : #p_object { description = '$1', id = '$3', interfaces = '$4', fields = '$5' }.
ObjectTypeDefinition -> Description 'type' Name ImplementsInterfaces Directives FieldsDefinition
  : #p_object { description = '$1', id = '$3', interfaces = '$4', directives = '$5', fields = '$6' }.

FieldsDefinition -> '{' '}' : [].
FieldsDefinition -> '{' FieldDefinitionList '}' : '$2'.

ImplementsInterfaces -> 'implements' '&' NamedType
  : ['$3'].
ImplementsInterfaces -> 'implements' NamedType
  : ['$2'].
ImplementsInterfaces -> ImplementsInterfaces '&' NamedType
  : ['$3'|'$1'].

FieldDefinitionList -> FieldDefinition : ['$1'].
FieldDefinitionList -> FieldDefinition FieldDefinitionList : ['$1'|'$2'].

FieldDefinition -> Name ':' Type
  : #p_field_def {id = '$1', type = '$3'}.
FieldDefinition -> Name ':' Type Directives
  : #p_field_def {id = '$1', type = '$3', directives = '$4'}.
FieldDefinition -> Name ArgumentsDefinition ':' Type
  : #p_field_def {id = '$1', args = '$2', type = '$4'}.
FieldDefinition -> Name ArgumentsDefinition ':' Type Directives
  : #p_field_def {id = '$1', args = '$2', type = '$4', directives = '$5'}.
FieldDefinition -> Description Name ':' Type
  : #p_field_def {description = '$1', id = '$2', type = '$4'}.
FieldDefinition -> Description Name ':' Type Directives
  : #p_field_def {description = '$1', id = '$2', type = '$4', directives = '$5'}.
FieldDefinition -> Description Name ArgumentsDefinition ':' Type
  : #p_field_def {description = '$1', id = '$2', args = '$3', type = '$5'}.
FieldDefinition -> Description Name ArgumentsDefinition ':' Type Directives
  : #p_field_def {description = '$1', id = '$2', args = '$3', type = '$5', directives = '$6'}.

ArgumentsDefinition -> '(' InputValueDefinitionList ')' : '$2'.

InputValueDefinitionList -> InputValueDefinition : ['$1'].
InputValueDefinitionList -> InputValueDefinition InputValueDefinitionList : ['$1'|'$2'].

InputValueDefinition -> Name ':' Type
  : #p_input_value {id = '$1', type = '$3'}.
InputValueDefinition -> Name ':' Type Directives
  : #p_input_value {id = '$1', type = '$3', directives = '$4'}.
InputValueDefinition -> Name ':' Type DefaultValue
  : #p_input_value {id = '$1', type = '$3', default = '$4'}.
InputValueDefinition -> Name ':' Type DefaultValue Directives
  : #p_input_value {id = '$1', type = '$3', default = '$4', directives = '$5'}.
InputValueDefinition -> Description Name ':' Type
  : #p_input_value {description = '$1', id = '$2', type = '$4'}.
InputValueDefinition -> Description Name ':' Type Directives
  : #p_input_value {description = '$1', id = '$2', type = '$4', directives = '$5'}.
InputValueDefinition -> Description Name ':' Type DefaultValue
  : #p_input_value {description = '$1', id = '$2', type = '$4', default = '$5'}.
InputValueDefinition -> Description Name ':' Type DefaultValue Directives
  : #p_input_value {description = '$1', id = '$2', type = '$4', default = '$5', directives = '$6'}.

InterfaceTypeDefinition -> 'interface' Name FieldsDefinition
  : #p_interface {id = '$2', fields = '$3'}.
InterfaceTypeDefinition -> 'interface' Name Directives FieldsDefinition
  : #p_interface {id = '$2', directives = '$3', fields = '$4'}.
InterfaceTypeDefinition -> Description 'interface' Name FieldsDefinition
  : #p_interface {description = '$1', id = '$3', fields = '$4'}.
InterfaceTypeDefinition -> Description 'interface' Name Directives FieldsDefinition
  : #p_interface {description = '$1', id = '$3', directives = '$4', fields = '$5'}.

UnionTypeDefinition -> 'union' Name UnionMemberTypes :
    #p_union {id = '$2', members = '$3'}.
UnionTypeDefinition -> 'union' Name Directives UnionMemberTypes :
    #p_union {id = '$2', directives = '$3', members = '$4'}.
UnionTypeDefinition -> Description 'union' Name UnionMemberTypes :
    #p_union {description = '$1', id = '$3', members = '$4'}.
UnionTypeDefinition -> Description 'union' Name Directives UnionMemberTypes :
    #p_union {description = '$1', id = '$3', directives = '$4', members = '$5'}.

UnionMemberTypes -> '=' NamedType : ['$2'].
UnionMemberTypes -> '=' '|' NamedType : ['$3'].
UnionMemberTypes -> UnionMemberTypes '|' NamedType
  : ['$3' | '$1'].
  
EnumTypeDefinition -> 'enum' Name EnumValuesDefinition
  : #p_enum {id = '$2', variants = '$3'}.
EnumTypeDefinition -> 'enum' Name Directives EnumValuesDefinition
  : #p_enum {id = '$2', directives = '$3', variants = '$4'}.
EnumTypeDefinition -> Description 'enum' Name EnumValuesDefinition
  : #p_enum {description = '$1', id = '$3', variants = '$4'}.
EnumTypeDefinition -> Description 'enum' Name Directives EnumValuesDefinition
  : #p_enum {description = '$1', id = '$3', directives = '$4', variants = '$5'}.

EnumValuesDefinition -> '{' EnumValueDefinitionList '}' : '$2'.

EnumValueDefinitionList -> EnumValueDefinition : ['$1'].
EnumValueDefinitionList -> EnumValueDefinition EnumValueDefinitionList : ['$1'|'$2'].

EnumValueDefinition -> EnumValue
  : #p_enum_value { id = '$1' }.
EnumValueDefinition -> EnumValue Directives
  : #p_enum_value { id = '$1', directives = '$2' }.
EnumValueDefinition -> Description EnumValue
  : #p_enum_value { description = '$1', id = '$2' }.
EnumValueDefinition -> Description EnumValue Directives
  : #p_enum_value { description = '$1', id = '$2', directives = '$3' }.


InputObjectTypeDefinition -> 'input' Name InputFieldsDefinition
  : #p_input_object{id = '$2', defs = '$3' }.
InputObjectTypeDefinition -> 'input' Name Directives InputFieldsDefinition
  : #p_input_object{id = '$2', directives = '$3', defs = '$4' }.
InputObjectTypeDefinition -> Description 'input' Name InputFieldsDefinition
  : #p_input_object{description = '$1', id = '$3', defs = '$4' }.
InputObjectTypeDefinition -> Description 'input' Name Directives InputFieldsDefinition
  : #p_input_object{description = '$1', id = '$3', directives = '$4', defs = '$5' }.

InputFieldsDefinition -> '{' InputValueDefinitionList '}'
  : '$2'.

Erlang code.

-include_lib("graphql/include/graphql.hrl").
-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

g_query({query, _L} = Q) -> Q.
g_mutation({mutation, _L} = Mut) -> Mut.
g_subscription({subscription, _L} = Sub) -> Sub.

g_ty({name, _, <<"String">>}) -> {scalar, <<"String">>};
g_ty({name, _, <<"string">>}) -> {scalar, <<"String">>};
g_ty({name, _, <<"Int">>}) -> {scalar, <<"Int">>};
g_ty({name, _, <<"int">>}) -> {scalar, <<"Int">>};
g_ty({name, _, <<"float">>}) -> {scalar, <<"Float">>};
g_ty({name, _, <<"Float">>}) -> {scalar, <<"Float">>};
g_ty({name, _, <<"bool">>}) -> {scalar, <<"Boolean">>};
g_ty({name, _, <<"Bool">>}) -> {scalar, <<"Boolean">>};
g_ty({name, _, <<"boolean">>}) -> {scalar, <<"Boolean">>};
g_ty({name, _, <<"Boolean">>}) -> {scalar, <<"Boolean">>};
g_ty({name, _, <<"id">>}) -> {scalar, <<"ID">>};
g_ty({name, _, <<"Id">>}) -> {scalar, <<"ID">>};
g_ty({name, _, <<"ID">>}) -> {scalar, <<"ID">>};
g_ty({name, _, _} = N) -> N.

g_enum({name, _Line, N}) -> N.

g_var({name, _, _} = N) -> {var, N}.
g_integer({int, _, I}) -> I.
g_float({float, _, F}) -> F.
g_string({bstring, _, S}) -> S.
g_bool({bool, _, B}) -> B.
g_null({null, _}) -> null.

g_list(L) when is_list(L) -> L.
g_input_object(KVPairs) ->
    {input_object, KVPairs}.

g_directive_location({name, _, <<"QUERY">>}) -> 'QUERY';
g_directive_location({name, _, <<"MUTATION">>}) -> 'MUTATION';
g_directive_location({name, _, <<"SUBSCRIPTION">>}) -> 'SUBSCRIPTION';
g_directive_location({name, _, <<"FIELD">>}) -> 'FIELD';
g_directive_location({name, _, <<"FRAGMENT_DEFINITION">>}) -> 'FRAGMENT_DEFINITION';
g_directive_location({name, _, <<"FRAGMENT_SPREAD">>}) -> 'FRAGMENT_SPREAD';
g_directive_location({name, _, <<"INLINE_FRAGMENT">>}) -> 'INLINE_FRAGMENT';
g_directive_location({name, _, <<"SCHEMA">>}) -> 'SCHEMA';
g_directive_location({name, _, <<"SCALAR">>}) -> 'SCALAR';
g_directive_location({name, _, <<"OBJECT">>}) -> 'OBJECT';
g_directive_location({name, _, <<"FIELD_DEFINITION">>}) -> 'FIELD_DEFINITION';
g_directive_location({name, _, <<"ARGUMENT_DEFINITION">>}) -> 'ARGUMENT_DEFINITION';
g_directive_location({name, _, <<"INTERFACE">>}) -> 'INTERFACE';
g_directive_location({name, _, <<"UNION">>}) -> 'UNION';
g_directive_location({name, _, <<"ENUM">>}) -> 'ENUM';
g_directive_location({name, _, <<"ENUM_VALUE">>}) -> 'ENUM_VALUE';
g_directive_location({name, _, <<"INPUT_OBJECT">>}) -> 'INPUT_OBJECT';
g_directive_location({name, _, <<"INPUT_FIELD_DEFINITION">>}) -> 'INPUT_FIELD_DEFINITION';
g_directive_location({name, Line, N}) -> return_error(Line, {invalid_directive_location, N}).



%% Convert keywords into binaries if they don't occur in the KW-position
keyword({A, Line}) when is_atom(A) ->
    {name, Line, atom_to_binary(A, utf8)}.
