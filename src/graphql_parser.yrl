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
OperationDefinition -> OperationType SelectionSet :
    #op { ty = '$1', selection_set = '$2' }.
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

Annotation -> '+' Name :
                  #annotation {
                     id = '$2',
                     args = []
                    }.
Annotation -> '+' Name Arguments :
                  #annotation {
                     id = '$2',
                     args = '$3'
                    }.

ObjectTypeDefinition -> AnnotationList 'type' Name '{' FieldDefinitionList '}' :
                            #p_type {
                               annotations = '$1',
                               id = '$3',
                               fields = '$5'
                              }.
ObjectTypeDefinition -> 'type' Name '{' FieldDefinitionList '}' :
                            #p_type {
                               id = '$2',
                               fields = '$4'
                              }.
ObjectTypeDefinition -> AnnotationList 'type' Name ImplementsInterfaces '{' FieldDefinitionList '}' :
                            #p_type {
                               annotations = '$1',
                               id = '$3',
                               implements = '$4',
                               fields = '$6'
                              }.
ObjectTypeDefinition -> 'type' Name ImplementsInterfaces '{' FieldDefinitionList '}' :
                            #p_type {
                               id = '$2',
                               implements = '$3',
                               fields = '$5'
                              }.

ImplementsInterfaces -> 'implements' NamedTypeList : '$2'.

NamedTypeList -> NamedType : ['$1'].
NamedTypeList -> NamedType NamedTypeList : ['$1'|'$2'].

FieldDefinitionList -> FieldDefinition : ['$1'].
FieldDefinitionList -> FieldDefinition FieldDefinitionList : ['$1'|'$2'].

FieldDefinition -> Name ':' Type :
                       #p_field_def {
                          id = '$1',
                          type = '$3'
                         }.
FieldDefinition -> Name ':' Type Directives :
                       #p_field_def {
                          id = '$1',
                          type = '$3',
                          directives = '$4'
                         }.
FieldDefinition -> Name ArgumentsDefinition ':' Type :
                       #p_field_def {
                          id = '$1',
                          args = '$2',
                          type = '$4'
                         }.
FieldDefinition -> Name ArgumentsDefinition ':' Type Directives :
                       #p_field_def {
                          id = '$1',
                          args = '$2',
                          type = '$4',
                          directives = '$5'
                         }.
FieldDefinition -> AnnotationList Name ':' Type :
                       #p_field_def {
                          annotations = '$1',
                          id = '$2',
                          type = '$4'
                         }.
FieldDefinition -> AnnotationList Name ':' Type Directives :
                       #p_field_def {
                          annotations = '$1',
                          id = '$2',
                          type = '$4',
                          directives = '$5'
                         }.
FieldDefinition -> AnnotationList Name ArgumentsDefinition ':' Type :
                       #p_field_def {
                          annotations = '$1',
                          id = '$2',
                          args = '$3',
                          type = '$5'
                         }.
FieldDefinition -> AnnotationList Name ArgumentsDefinition ':' Type Directives :
                       #p_field_def {
                          annotations = '$1',
                          id = '$2',
                          args = '$3',
                          type = '$5',
                          directives = '$6'
                         }.

ArgumentsDefinition -> '(' InputValueDefinitionList ')' : '$2'.

InputValueDefinitionList -> InputValueDefinition : ['$1'].
InputValueDefinitionList -> InputValueDefinition InputValueDefinitionList : ['$1'|'$2'].


InputValueDefinition -> AnnotationList Name ':' Type :
                            #p_input_value {
                               annotations = '$1',
                               id = '$2',
                               type = '$4'
                              }.
InputValueDefinition -> Name ':' Type :
                            #p_input_value {
                               id = '$1',
                               type = '$3'
                              }.
InputValueDefinition -> AnnotationList Name ':' Type DefaultValue :
                            #p_input_value {
                               annotations = '$1',
                               id = '$2',
                               type = '$4',
                               default = '$5'
                              }.
InputValueDefinition -> Name ':' Type DefaultValue :
                            #p_input_value {
                               id = '$1',
                               type = '$3',
                               default = '$4'
                              }.

InterfaceTypeDefinition -> AnnotationList 'interface' Name '{' FieldDefinitionList '}' :
                               #p_interface {
                                  annotations = '$1',
                                  id = '$3',
                                  fields = '$5'
                                 }.
InterfaceTypeDefinition -> 'interface' Name '{' FieldDefinitionList '}' :
                               #p_interface {
                                  id = '$2',
                                  fields = '$4'
                                 }.

UnionTypeDefinition -> AnnotationList 'union' Name '=' UnionMembers :
                           #p_union {
                              annotations = '$1',
                              id = '$3',
                              members = '$5'
                             }.
UnionTypeDefinition -> 'union' Name '=' UnionMembers :
    #p_union {
       id = '$2',
       members = '$4'
      }.

UnionMembers -> NamedType : ['$1'].
UnionMembers -> NamedType '|' UnionMembers : ['$1'|'$3'].

ScalarTypeDefinition -> AnnotationList 'scalar' Name :
                            #p_scalar { 
                               id = '$3',
                               annotations = '$1' 
                              }.
ScalarTypeDefinition -> 'scalar' Name :
                            #p_scalar {
                               id = '$2'
                              }.

EnumTypeDefinition -> AnnotationList 'enum' Name '{' EnumValueDefinitionList '}' :
                          #p_enum {
                             annotations = '$1',
                             id = '$3',
                             variants = '$5'
                            }.
EnumTypeDefinition -> 'enum' Name '{' EnumValueDefinitionList '}' :
                          #p_enum {
                             id = '$2',
                             variants = '$4'
                            }.

EnumValueDefinitionList -> EnumValueDefinition : ['$1'].
EnumValueDefinitionList -> EnumValueDefinition EnumValueDefinitionList : ['$1'|'$2'].

EnumValueDefinition -> EnumValue :
    #p_enum_value { id = '$1' }.
EnumValueDefinition -> AnnotationList EnumValue :
    #p_enum_value { id = '$2',
                    annotations = '$1' }.

InputObjectTypeDefinition -> AnnotationList 'input' Name '{' InputValueDefinitionList '}' :
                                 #p_input_object{
                                    id = '$3',
                                    defs = '$5',
                                    annotations = '$1' }.
InputObjectTypeDefinition -> 'input' Name '{' InputValueDefinitionList '}' :
                                 #p_input_object{
                                    id = '$2',
                                    defs = '$4',
                                    annotations = [] }.

TypeExtensionDefinition -> 'extend' ObjectTypeDefinition :
    {extend, '$2'}.

Erlang code.

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

g_query({query, _L} = Q) -> Q.
g_mutation({mutation, _L} = Mut) -> Mut.
g_subscription({subscription, _L} = Sub) -> Sub.

g_ty({name, _, <<"String">>}) -> wrap_scalar(<<"String">>);
g_ty({name, _, <<"string">>}) -> wrap_scalar(<<"String">>);
g_ty({name, _, <<"Int">>}) -> wrap_scalar(<<"Int">>);
g_ty({name, _, <<"int">>}) -> wrap_scalar(<<"Int">>);
g_ty({name, _, <<"float">>}) -> wrap_scalar(<<"Float">>);
g_ty({name, _, <<"Float">>}) -> wrap_scalar(<<"Float">>);
g_ty({name, _, <<"bool">>}) -> wrap_scalar(<<"Bool">>);
g_ty({name, _, <<"Bool">>}) -> wrap_scalar(<<"Bool">>);
g_ty({name, _, <<"boolean">>}) -> wrap_scalar(<<"Bool">>);
g_ty({name, _, <<"Boolean">>}) -> wrap_scalar(<<"Bool">>);
g_ty({name, _, <<"id">>}) -> wrap_scalar(<<"ID">>);
g_ty({name, _, <<"Id">>}) -> wrap_scalar(<<"ID">>);
g_ty({name, _, <<"ID">>}) -> wrap_scalar(<<"ID">>);
g_ty({name, _, _} = N) -> N.

g_enum({name, _Line, N}) -> N.

g_var({name, _, _} = N) -> {var, N}.
g_integer({int, _, I}) -> I.
g_float({float, _, F}) -> F.
g_string({bstring, _, S}) -> S.
g_bool({bool, _, B}) -> B.

g_list(L) when is_list(L) -> L.
g_input_object(KVPairs) ->
    {input_object, KVPairs}.

%% Convert keywords into binaries if they don't occur in the KW-position
keyword({A, Line}) when is_atom(A) ->
    {name, Line, atom_to_binary(A, utf8)}.

wrap_scalar(Name) ->
    {scalar, Name}.
