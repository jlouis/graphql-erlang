-module(graphql_schema_canonicalize).

-include("graphql_schema.hrl").

-export([x/1]).

-spec x(any()) -> any().
x({root, Root}) ->
    Q = root_query(Root),
    M = root_mutation(Root),
    S = root_subscription(Root),
    IFs = root_interfaces(Root),
    #root_schema { query = Q, mutation = M, subscription = S, interfaces = IFs };
x({interface, #{ id := ID, description := Desc, fields := FieldDefs } = I}) ->
    Resolver = interface_resolver(I),
    #interface_type { id = c_id(ID), description = binarize(Desc),
        resolve_type = Resolver,
        annotations = annotations(I),
        fields = map_2(fun c_field/2, FieldDefs) };
x({union, #{ id := ID, description := Desc, types := Types } = U}) ->
    Resolver = union_resolver(U),
    #union_type {
        id = c_id(ID),
        description = binarize(Desc),
        annotations = annotations(U),
        types = [c_type(T) || T <- Types],
        resolve_type = Resolver
    };

x({enum, #{ id := ID, description := Desc, values := VDefs} = Enum}) ->
    ModuleResolver = enum_resolve(Enum),
    #enum_type {
        id = c_id(ID),
        description = binarize(Desc),
        annotations = annotations(Enum),
        values = map_2(fun c_enum_val/2, VDefs),
        resolve_module = ModuleResolver
     };

x({object, #{ id := ID, fields := FieldDefs, description := Desc } = Obj}) ->
    Interfaces = c_interfaces(Obj),
    ModuleResolver = maps:get(resolve_module, Obj, undefined),
    #object_type {
        id = c_id(ID),
        resolve_module = ModuleResolver,
        description = binarize(Desc),
        annotations = annotations(Obj),
        fields = map_2(fun c_field/2, FieldDefs),
        interfaces = Interfaces
    };
x({input_object, #{ id := ID, fields := FieldDefs, description := Desc } = IO}) ->
    #input_object_type { id = c_id(ID), description = binarize(Desc),
         annotations = annotations(IO),
         fields = map_2(fun c_input_value/2, FieldDefs) };

%% Scalar--START

x({scalar, #{ id := ID, description := Desc} = Scalar}) ->
    ModuleResolver = scalar_resolve(Scalar),
    #scalar_type {
       id = c_id(ID),
       description = binarize(Desc),
       annotations = annotations(Scalar),
       resolve_module = ModuleResolver }.

%% -- ROOT -------------
root_query(#{ query := Q}) -> binarize(Q);
root_query(_) -> undefined.

root_mutation(#{ mutation := M}) -> binarize(M);
root_mutation(_) -> undefined.

root_subscription(#{ subscription := S}) -> binarize(S);
root_subscription(_) -> undefined.

root_interfaces(#{ interfaces := IFs }) -> [binarize(I) || I <- IFs];
root_interfaces(_) -> [].

c_id(ID) -> binarize(ID).
c_type(ID) -> binarize(ID).

c_interfaces(#{ interfaces := IFs }) -> [binarize(I) || I <- IFs];
c_interfaces(#{}) -> [].

c_enum_val(K, #{ value := V, description := Desc } = Map) ->
    Key = binarize(K),
    {V, #enum_value {
        val = Key,
        description = binarize(Desc),
        deprecation = deprecation(Map) }}.


%% -- FIELDS ----------

c_input_value(K, V) ->
    {binarize(K), c_input_value_val(V)}.

c_input_value_val(#{ type := Ty, description := Desc } = M) ->
    Def = default(M),
    #schema_arg {
        ty = handle_type(Ty),
        default = Def,
        description = binarize(Desc)
    }.

default(#{ default := Def }) -> Def;
default(#{ }) -> null.

c_field(K, V) ->
    {binarize(K), c_field_val(V)}.

c_field_val(M) ->
    #schema_field {
        ty = c_field_val_ty(M),
        resolve = c_field_val_resolve(M),
        args = c_field_val_args(M),
        deprecation = deprecation(M),
        annotations = annotations(M),
        description = c_field_val_description(M)
    }.

c_field_val_ty(#{ type := Ty }) ->
    handle_type(Ty).

handle_type({non_null, Ty}) -> {non_null, handle_type(Ty)};
handle_type([Ty]) -> {list, handle_type(Ty)};
handle_type({list, Ty}) -> {list, handle_type(Ty)};
handle_type(A) when is_atom(A) -> non_null(atom_to_list(A)).

non_null(Ty) ->
    case lists:reverse(Ty) of
        [$! | Rest] -> {non_null, list_to_binary(lists:reverse(Rest))};
        _Otherwise -> list_to_binary(Ty)
    end.

c_field_val_resolve(#{ resolve := R}) when is_function(R, 3) -> R;
c_field_val_resolve(#{ resolve := R}) when is_function(R) -> exit(wrong_resolver_arity);
c_field_val_resolve(_) -> undefined.

c_field_val_args(#{ args := Args }) ->map_2(fun c_args/2, Args);
c_field_val_args(_) -> #{}.

c_field_val_description(#{ description := D }) -> iolist_to_binary(D);
c_field_val_description(_) -> undefined.

%% -- Args
c_args(K, V) ->
    {binarize(K), c_arg_val(V)}.


c_arg_val(#{ type := Ty, description := Desc, default := Def }) ->
    #schema_arg { ty = handle_type(Ty), description = binarize(Desc), default = Def };
c_arg_val(#{ type := Ty, description := Desc }) ->
    #schema_arg { ty = handle_type(Ty), description = binarize(Desc) }.

union_resolver(#{ resolve_module := M}) when is_atom(M) -> M;
union_resolver(#{ resolve_type := F}) when is_function(F,1) -> F;
union_resolver(#{ id := ID }) -> fun(_Data) -> exit({no_union_resolver, ID}) end.

interface_resolver(#{ resolve_module := M }) when is_atom(M) -> M;
interface_resolver(#{ resolve_type := F }) when is_function(F,1) -> F;
interface_resolver(#{ id := ID }) -> fun(_Data) -> exit({no_interface_resolver, ID}) end.

binarize(A) when is_atom(A) -> atom_to_binary(A, utf8);
binarize(S) when is_list(S) -> list_to_binary(S);
binarize(B) when is_binary(B) -> B.

map_2(F, M) ->
    Unpacked = maps:to_list(M),
    maps:from_list([F(K, V) || {K, V} <- Unpacked]).

scalar_resolve(#{ resolve_module := ModuleResolver }) when is_atom(ModuleResolver) ->
    ModuleResolver;

scalar_resolve(#{ id := 'ID'}) ->
     graphql_scalar_binary_coerce;

scalar_resolve(#{ id := 'String'}) ->
     graphql_scalar_binary_coerce;

scalar_resolve(#{ id := 'Bool'}) ->
     graphql_scalar_bool_coerce;

scalar_resolve(#{ id := 'Int'}) ->
     graphql_scalar_integer_coerce;

scalar_resolve(#{ id := 'Float'}) ->
     graphql_scalar_float_coerce;

scalar_resolve(_) ->
     graphql_enum_coerce.

enum_resolve(#{ resolve_module := ModuleResolver }) when is_atom(ModuleResolver) ->
    ModuleResolver;
enum_resolve(_) ->
    graphql_enum_coerce.

%% -- Annotations
annotations(#{ annotations := Annots }) -> Annots;
annotations(#{}) -> #{}.

%% -- Deprecation
deprecation(#{ deprecation := Reason }) ->
    binarize(Reason);
deprecation(#{}) -> undefined.
