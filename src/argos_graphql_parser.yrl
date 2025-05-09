Header "%%%-----------------------------------------------------------------------------"
"%%% Copyright (c) Meta Platforms, Inc. and affiliates."
"%%% Copyright (c) WhatsApp LLC"
"%%%"
"%%% This source code is licensed under the MIT license found in the"
"%%% LICENSE.md file in the root directory of this source tree."
"%%%"
"%%% @author Andrew Bennett <potatosaladx@meta.com>"
"%%% @copyright (c) Meta Platforms, Inc. and affiliates."
"%%% @doc GraphQL Parser"
"%%%"
"%%% See the spec reference"
"%%% https://spec.graphql.org/draft/#sec-Appendix-Grammar-Summary"
"%%%"
"%%% @end"
"%%% Created :  10 Nov 2023 by Andrew Bennett <potatosaladx@meta.com>"
"%%% Modified : 09 May 2025 by Eric Pailleau <argos@crownedgrouse.com> for argos project use."
"%%%            This is not the original code which can be found at : "
"%%%            https://github.com/WhatsApp/erlang-argo/blob/main/apps/argo/src/graphql/language/argo_graphql_language_parser.yrl "
"%%%-----------------------------------------------------------------------------"
"%%% % @format".

Nonterminals
  Arguments ArgumentList Argument
  ArgumentsConst ArgumentConstList ArgumentConst
  ArgumentsDefinition InputValueDefinitionList InputValueDefinition
  DefinitionList Definition
  Directives DirectiveList Directive
  DirectivesConst DirectiveConstList DirectiveConst
  DefaultValue
  DescriptionDefinition
  DirectiveDefinition DirectiveLocations DirectiveLocation ExecutableDirectiveLocation TypeSystemDirectiveLocation
  Document
  EnumTypeDefinition EnumValuesDefinition EnumValueDefinitionList EnumValueDefinition
  EnumTypeExtension
  ExecutableDefinition
  Field Alias
  FieldsDefinition FieldDefinitionList FieldDefinition
  FragmentDefinition FragmentName TypeCondition
  FragmentSpread
  ImplementsInterfaces ImplementsInterfacesList
  InlineFragment
  InputObjectTypeDefinition InputFieldsDefinition
  InputObjectTypeExtension
  InterfaceTypeDefinition
  InterfaceTypeExtension
  NameWithoutOn Name
  ObjectTypeDefinition
  ObjectTypeExtension
  OperationDefinition
  OperationType
  RootOperationTypesDefinition RootOperationTypeDefinitionList RootOperationTypeDefinition
  ScalarTypeDefinition
  ScalarTypeExtension
  SchemaDefinition
  SchemaExtension
  SelectionSet SelectionList Selection
  Type NamedType ListType NonNullType
  TypeDefinition
  TypeExtension
  TypeSystemDefinition
  TypeSystemExtension
  UnionTypeDefinition UnionMemberTypes
  UnionTypeExtension
  Value EnumValue ListValue ValueList ObjectValue ObjectFieldList ObjectField
  ValueConst ListValueConst ValueConstList ObjectValueConst ObjectFieldConstList ObjectFieldConst
  VariablesDefinition VariableDefinitionList VariableDefinition Variable.

Terminals
  '{' '}' '(' ')' '[' ']' '!' ':' '@' '$' '=' '|' '&' '...'
  'query' 'mutation' 'subscription' 'fragment' 'on' 'directive' 'repeatable'
  'type' 'implements' 'interface' 'union' 'scalar' 'enum' 'input' 'extend' 'schema'
  'executable_directive_location' 'type_system_directive_location'
  name int_value float_value string_value block_string_value boolean_value null.

Rootsymbol Document.

Arguments -> '(' ArgumentList ')' : parse(#{'arguments' => '$2'}, extract_location('$2')).
ArgumentList -> Argument : ['$1'].
ArgumentList -> Argument ArgumentList : ['$1'|'$2'].
Argument -> NameWithoutOn ':' Value : parse(#{'name' => extract_binary('$1'), 'value' => '$3'}, extract_location('$1')).
Argument -> 'on' ':' Value : parse(#{'name' => extract_binary('$1'), 'value' => '$3'}, extract_location('$1')).

ArgumentsConst -> '(' ArgumentConstList ')' : parse(#{'arguments' => '$2'}, extract_location('$2')).
ArgumentConstList -> ArgumentConst : ['$1'].
ArgumentConstList -> ArgumentConst ArgumentConstList : ['$1'|'$2'].
ArgumentConst -> NameWithoutOn ':' ValueConst : parse(#{name => extract_binary('$1'), value => '$3'}, extract_location('$1')).
ArgumentConst -> 'on' ':' ValueConst : parse(#{name => extract_binary('$1'), value => '$3'}, extract_location('$1')).

ArgumentsDefinition -> '(' InputValueDefinitionList ')' : parse(#{'inputs' => '$2'}, extract_location('$1')).
InputValueDefinitionList -> InputValueDefinition : ['$1'].
InputValueDefinitionList -> InputValueDefinition InputValueDefinitionList : ['$1'|'$2'].
InputValueDefinition -> Name ':' Type : parse(#{'name' => extract_binary('$1'), 'type' => '$3'}, extract_location('$1')).
InputValueDefinition -> Name ':' Type DirectivesConst : parse(#{'name' => extract_binary('$1'), 'type' => '$3', 'directives' => '$4'}, extract_location('$1')).
InputValueDefinition -> Name ':' Type DefaultValue : parse(#{'name' => extract_binary('$1'), 'type' => '$3', 'default_value' => '$4'}, extract_location('$1')).
InputValueDefinition -> Name ':' Type DefaultValue DirectivesConst : parse(#{'name' => extract_binary('$1'), 'type' => '$3', 'default_value' => '$4', 'directives' => '$5'}, extract_location('$1')).
InputValueDefinition -> DescriptionDefinition Name ':' Type : parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'type' => '$4'}, extract_location('$2')).
InputValueDefinition -> DescriptionDefinition Name ':' Type DirectivesConst : parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'type' => '$4', 'directives' => '$5'}, extract_location('$2')).
InputValueDefinition -> DescriptionDefinition Name ':' Type DefaultValue : parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'type' => '$4', 'default_value' => '$5'}, extract_location('$2')).
InputValueDefinition -> DescriptionDefinition Name ':' Type DefaultValue DirectivesConst : parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'type' => '$4', 'default_value' => '$5', 'directives' => '$6'}, extract_location('$2')).

DefaultValue -> '=' ValueConst : '$2'.

DefinitionList -> Definition : ['$1'].
DefinitionList -> Definition DefinitionList : ['$1'|'$2'].
Definition -> ExecutableDefinition : executable_definition('$1', extract_location('$1')).
Definition -> TypeSystemDefinition : type_system_definition('$1', extract_location('$1')).
Definition -> TypeSystemExtension : type_system_extension('$1', extract_location('$1')).

DescriptionDefinition -> string_value : extract_quoted_string_token('$1').
DescriptionDefinition -> block_string_value : extract_quoted_block_string_token('$1').

Directives -> DirectiveList : parse(#{'directives' => '$1'}, extract_location('$1')).
DirectiveList -> Directive : ['$1'].
DirectiveList -> Directive DirectiveList : ['$1'|'$2'].
Directive -> '@' NameWithoutOn : parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
Directive -> '@' NameWithoutOn Arguments : parse(#{'name' => extract_binary('$2'), 'arguments' => '$3'}, extract_location('$1')).
Directive -> '@' 'on' : parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
Directive -> '@' 'on' Arguments : parse(#{'name' => extract_binary('$2'), 'arguments' => '$3'}, extract_location('$1')).

DirectivesConst -> DirectiveConstList : parse(#{'directives' => '$1'}, extract_location('$1')).
DirectiveConstList -> DirectiveConst : ['$1'].
DirectiveConstList -> DirectiveConst DirectiveConstList : ['$1'|'$2'].
DirectiveConst -> '@' NameWithoutOn : parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
DirectiveConst -> '@' NameWithoutOn ArgumentsConst : parse(#{'name' => extract_binary('$2'), 'arguments' => '$3'}, extract_location('$1')).
DirectiveConst -> '@' 'on' : parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
DirectiveConst -> '@' 'on' ArgumentsConst : parse(#{'name' => extract_binary('$2'), 'arguments' => '$3'}, extract_location('$1')).

DirectiveDefinition -> 'directive' '@' Name 'on' DirectiveLocations : parse(#{'name' => extract_binary('$3'), 'locations' => '$5'}, extract_location('$1')).
DirectiveDefinition -> 'directive' '@' Name ArgumentsDefinition 'on' DirectiveLocations : parse(#{'name' => extract_binary('$3'), 'arguments' => '$4', 'locations' => '$6'}, extract_location('$1')).
DirectiveDefinition -> 'directive' '@' Name 'repeatable' 'on' DirectiveLocations : parse(#{'name' => extract_binary('$3'), 'locations' => '$6', 'repeatable' => true}, extract_location('$1')).
DirectiveDefinition -> 'directive' '@' Name ArgumentsDefinition 'repeatable' 'on' DirectiveLocations : parse(#{'name' => extract_binary('$3'), 'arguments' => '$4', 'locations' => '$7', 'repeatable' => true}, extract_location('$1')).
DirectiveDefinition -> DescriptionDefinition 'directive' '@' Name 'on' DirectiveLocations : parse(#{'description' => '$1', 'name' => extract_binary('$4'), 'locations' => '$6'}, extract_location('$2')).
DirectiveDefinition -> DescriptionDefinition 'directive' '@' Name ArgumentsDefinition 'on' DirectiveLocations : parse(#{'description' => '$1', 'name' => extract_binary('$4'), 'arguments' => '$5', 'locations' => '$7'}, extract_location('$2')).
DirectiveDefinition -> DescriptionDefinition 'directive' '@' Name 'repeatable' 'on' DirectiveLocations : parse(#{'description' => '$1', 'name' => extract_binary('$4'), 'locations' => '$7', 'repeatable' => true}, extract_location('$2')).
DirectiveDefinition -> DescriptionDefinition 'directive' '@' Name ArgumentsDefinition 'repeatable' 'on' DirectiveLocations : parse(#{'description' => '$1', 'name' => extract_binary('$4'), 'arguments' => '$5', 'locations' => '$8', 'repeatable' => true}, extract_location('$2')).
DirectiveLocations -> DirectiveLocation : ['$1'].
DirectiveLocations -> DirectiveLocation '|' DirectiveLocations : ['$1'|'$3'].
DirectiveLocations -> '|' DirectiveLocation '|' DirectiveLocations : ['$2'|'$4'].
DirectiveLocation -> ExecutableDirectiveLocation : executable_directive_location('$1', extract_location('$1')).
DirectiveLocation -> TypeSystemDirectiveLocation : type_system_directive_location('$1', extract_location('$1')).
ExecutableDirectiveLocation -> 'executable_directive_location' : parse(#{'name' => extract_atom('$1')}, extract_location('$1')).
TypeSystemDirectiveLocation -> 'type_system_directive_location' : parse(#{'name' => extract_atom('$1')}, extract_location('$1')).

Document -> DefinitionList : parse(#{'definitions' => '$1'}, extract_location('$1')).

EnumTypeDefinition -> 'enum' Name : parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
EnumTypeDefinition -> 'enum' Name DirectivesConst : parse(#{'name' => extract_binary('$2'), 'directives' => '$3'}, extract_location('$1')).
EnumTypeDefinition -> 'enum' Name EnumValuesDefinition : parse(#{'name' => extract_binary('$2'), 'values' => '$3'}, extract_location('$1')).
EnumTypeDefinition -> 'enum' Name DirectivesConst EnumValuesDefinition : parse(#{'name' => extract_binary('$2'), 'directives' => '$3', 'values' => '$4'}, extract_location('$1')).
EnumTypeDefinition -> DescriptionDefinition 'enum' Name : parse(#{'description' => '$1', 'name' => extract_binary('$3')}, extract_location('$2')).
EnumTypeDefinition -> DescriptionDefinition 'enum' Name DirectivesConst : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$2')).
EnumTypeDefinition -> DescriptionDefinition 'enum' Name EnumValuesDefinition : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'values' => '$4'}, extract_location('$2')).
EnumTypeDefinition -> DescriptionDefinition 'enum' Name DirectivesConst EnumValuesDefinition : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4', 'values' => '$5'}, extract_location('$2')).
EnumValuesDefinition -> '{' '}' : parse(#{'values' => []}, extract_location('$1')).
EnumValuesDefinition -> '{' EnumValueDefinitionList '}' : parse(#{'values' => '$2'}, extract_location('$1')).
EnumValueDefinitionList -> EnumValueDefinition : ['$1'].
EnumValueDefinitionList -> EnumValueDefinition EnumValueDefinitionList : ['$1'|'$2'].
EnumValueDefinition -> EnumValue : parse(#{'value' => '$1'}, extract_location('$1')).
EnumValueDefinition -> EnumValue DirectivesConst : parse(#{'value' => '$1', 'directives' => '$2'}, extract_location('$1')).
EnumValueDefinition -> DescriptionDefinition EnumValue : parse(#{'description' => '$1', 'value' => '$2'}, extract_location('$2')).
EnumValueDefinition -> DescriptionDefinition EnumValue DirectivesConst : parse(#{'description' => '$1', 'value' => '$2', 'directives' => '$3'}, extract_location('$2')).

EnumTypeExtension -> 'extend' 'enum' Name DirectivesConst : parse(#{'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$1')).
EnumTypeExtension -> 'extend' 'enum' Name EnumValuesDefinition : parse(#{'name' => extract_binary('$3'), 'values' => '$4'}, extract_location('$1')).
EnumTypeExtension -> 'extend' 'enum' Name DirectivesConst EnumValuesDefinition : parse(#{'name' => extract_binary('$3'), 'directives' => '$4', 'values' => '$5'}, extract_location('$1')).

ExecutableDefinition -> OperationDefinition : operation_definition('$1', extract_location('$1')).
ExecutableDefinition -> FragmentDefinition : fragment_definition('$1', extract_location('$1')).

Field -> Name : parse(#{'name' => extract_binary('$1')}, extract_location('$1')).
Field -> Name Arguments : parse(#{'name' => extract_binary('$1'), 'arguments' => '$2'}, extract_location('$1')).
Field -> Name Directives : parse(#{'name' => extract_binary('$1'), 'directives' => '$2'}, extract_location('$1')).
Field -> Name SelectionSet : parse(#{'name' => extract_binary('$1'), 'selection_set' => '$2'}, extract_location('$1')).
Field -> Name Directives SelectionSet : parse(#{'name' => extract_binary('$1'), 'directives' => '$2', 'selection_set' => '$3'}, extract_location('$1')).
Field -> Name Arguments SelectionSet : parse(#{'name' => extract_binary('$1'), 'arguments' => '$2', 'selection_set' => '$3'}, extract_location('$1')).
Field -> Name Arguments Directives : parse(#{'name' => extract_binary('$1'), 'arguments' => '$2', 'directives' => '$3'}, extract_location('$1')).
Field -> Name Arguments Directives SelectionSet : parse(#{'name' => extract_binary('$1'), 'arguments' => '$2', 'directives' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
Field -> Alias Name : parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2')}, extract_location('$1')).
Field -> Alias Name Arguments : parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2'), 'arguments' => '$3'}, extract_location('$1')).
Field -> Alias Name SelectionSet : parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2'), 'selection_set' => '$3'}, extract_location('$1')).
Field -> Alias Name Arguments SelectionSet : parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2'), 'arguments' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
Field -> Alias Name Directives : parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2'), 'directives' => '$3'}, extract_location('$1')).
Field -> Alias Name Arguments Directives : parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2'), 'arguments' => '$3', 'directives' => '$4'}, extract_location('$1')).
Field -> Alias Name Directives SelectionSet : parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2'), 'directives' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
Field -> Alias Name Arguments Directives SelectionSet : parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2'), 'arguments' => '$3', 'directives' => '$4', 'selection_set' => '$5'}, extract_location('$1')).
Alias -> Name ':' : '$1'.

FieldsDefinition -> '{' '}' : parse(#{'fields' => []}, extract_location('$1')).
FieldsDefinition -> '{' FieldDefinitionList '}' : parse(#{'fields' => '$2'}, extract_location('$1')).
FieldDefinitionList -> FieldDefinition : ['$1'].
FieldDefinitionList -> FieldDefinition FieldDefinitionList : ['$1'|'$2'].
FieldDefinition -> Name ':' Type : parse(#{'name' => extract_binary('$1'), 'type' => '$3'}, extract_location('$1')).
FieldDefinition -> Name ':' Type DirectivesConst : parse(#{'name' => extract_binary('$1'), 'type' => '$3', 'directives' => '$4'}, extract_location('$1')).
FieldDefinition -> Name ArgumentsDefinition ':' Type : parse(#{'name' => extract_binary('$1'), 'arguments' => '$2', 'type' => '$4'}, extract_location('$1')).
FieldDefinition -> Name ArgumentsDefinition ':' Type DirectivesConst : parse(#{'name' => extract_binary('$1'), 'arguments' => '$2', 'type' => '$4', 'directives' => '$5'}, extract_location('$1')).
FieldDefinition -> DescriptionDefinition Name ':' Type : parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'type' => '$4'}, extract_location('$2')).
FieldDefinition -> DescriptionDefinition Name ':' Type DirectivesConst : parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'type' => '$4', 'directives' => '$5'}, extract_location('$2')).
FieldDefinition -> DescriptionDefinition Name ArgumentsDefinition ':' Type : parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'arguments' => '$3', 'type' => '$5'}, extract_location('$2')).
FieldDefinition -> DescriptionDefinition Name ArgumentsDefinition ':' Type DirectivesConst : parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'arguments' => '$3', 'type' => '$5', 'directives' => '$6'}, extract_location('$2')).

FragmentDefinition -> 'fragment' FragmentName TypeCondition SelectionSet : parse(#{'name' => '$2', 'type_condition' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
FragmentDefinition -> 'fragment' FragmentName TypeCondition Directives SelectionSet : parse(#{'name' => '$2', 'type_condition' => '$3', 'directives' => '$4', 'selection_set' => '$5'}, extract_location('$1')).
FragmentName -> NameWithoutOn : parse(#{'name' => extract_binary('$1')}, extract_location('$1')).
TypeCondition -> 'on' NamedType : parse(#{'type' => '$2'}, extract_location('$1')).

FragmentSpread -> '...' FragmentName : parse(#{'name' => '$2'}, extract_location('$1')).
FragmentSpread -> '...' FragmentName Directives : parse(#{'name' => '$2', 'directives' => '$3'}, extract_location('$1')).

ImplementsInterfaces -> 'implements' ImplementsInterfacesList : parse(#{'interfaces' => '$2'}, extract_location('$1')).
ImplementsInterfacesList -> NamedType : ['$1'].
ImplementsInterfacesList -> NamedType '&' ImplementsInterfacesList : ['$1'|'$3'].
ImplementsInterfacesList -> '&' NamedType '&' ImplementsInterfacesList : ['$2'|'$4'].

InlineFragment -> '...' TypeCondition SelectionSet : parse(#{'type_condition' => '$2', 'selection_set' => '$3'}, extract_location('$1')).
InlineFragment -> '...' TypeCondition Directives SelectionSet : parse(#{'type_condition' => '$2', 'directives' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
InlineFragment -> '...' Directives SelectionSet : parse(#{'directives' => '$2', 'selection_set' => '$3'}, extract_location('$1')).
InlineFragment -> '...' SelectionSet : parse(#{'selection_set' => '$2'}, extract_location('$1')).

InputObjectTypeDefinition -> 'input' Name : parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
InputObjectTypeDefinition -> 'input' Name DirectivesConst : parse(#{'name' => extract_binary('$2'), 'directives' => '$3'}, extract_location('$1')).
InputObjectTypeDefinition -> 'input' Name InputFieldsDefinition : parse(#{'name' => extract_binary('$2'), 'fields' => '$3'}, extract_location('$1')).
InputObjectTypeDefinition -> 'input' Name DirectivesConst InputFieldsDefinition : parse(#{'name' => extract_binary('$2'), 'directives' => '$3', 'fields' => '$4'}, extract_location('$1')).
InputObjectTypeDefinition -> DescriptionDefinition 'input' Name : parse(#{'description' => '$1', 'name' => extract_binary('$3')}, extract_location('$2')).
InputObjectTypeDefinition -> DescriptionDefinition 'input' Name DirectivesConst : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$2')).
InputObjectTypeDefinition -> DescriptionDefinition 'input' Name InputFieldsDefinition : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'fields' => '$4'}, extract_location('$2')).
InputObjectTypeDefinition -> DescriptionDefinition 'input' Name DirectivesConst InputFieldsDefinition : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4', 'fields' => '$5'}, extract_location('$2')).
InputFieldsDefinition -> '{' '}' : parse(#{'inputs' => []}, extract_location('$1')).
InputFieldsDefinition -> '{' InputValueDefinitionList '}' : parse(#{'inputs' => '$2'}, extract_location('$1')).

InputObjectTypeExtension -> 'extend' 'input' Name DirectivesConst : parse(#{'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$1')).
InputObjectTypeExtension -> 'extend' 'input' Name InputFieldsDefinition : parse(#{'name' => extract_binary('$3'), 'fields' => '$4'}, extract_location('$1')).
InputObjectTypeExtension -> 'extend' 'input' Name DirectivesConst InputFieldsDefinition : parse(#{'name' => extract_binary('$3'), 'directives' => '$4', 'fields' => '$5'}, extract_location('$1')).

InterfaceTypeDefinition -> 'interface' Name : parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
InterfaceTypeDefinition -> 'interface' Name ImplementsInterfaces : parse(#{'name' => extract_binary('$2'), 'implements' => '$3'}, extract_location('$1')).
InterfaceTypeDefinition -> 'interface' Name DirectivesConst : parse(#{'name' => extract_binary('$2'), 'directives' => '$3'}, extract_location('$1')).
InterfaceTypeDefinition -> 'interface' Name FieldsDefinition : parse(#{'name' => extract_binary('$2'), 'fields' => '$3'}, extract_location('$1')).
InterfaceTypeDefinition -> 'interface' Name ImplementsInterfaces DirectivesConst : parse(#{'name' => extract_binary('$2'), 'implements' => '$3', 'directives' => '$4'}, extract_location('$1')).
InterfaceTypeDefinition -> 'interface' Name ImplementsInterfaces FieldsDefinition : parse(#{'name' => extract_binary('$2'), 'implements' => '$3', 'fields' => '$4'}, extract_location('$1')).
InterfaceTypeDefinition -> 'interface' Name DirectivesConst FieldsDefinition : parse(#{'name' => extract_binary('$2'), 'directives' => '$3', 'fields' => '$4'}, extract_location('$1')).
InterfaceTypeDefinition -> 'interface' Name ImplementsInterfaces DirectivesConst FieldsDefinition : parse(#{'name' => extract_binary('$2'), 'implements' => '$3', 'directives' => '$4', 'fields' => '$5'}, extract_location('$1')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name : parse(#{'description' => '$1', 'name' => extract_binary('$3')}, extract_location('$2')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name ImplementsInterfaces : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4'}, extract_location('$2')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name DirectivesConst : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$2')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name FieldsDefinition : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'fields' => '$4'}, extract_location('$2')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name ImplementsInterfaces DirectivesConst : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5'}, extract_location('$2')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name ImplementsInterfaces FieldsDefinition : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4', 'fields' => '$5'}, extract_location('$2')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name DirectivesConst FieldsDefinition : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4', 'fields' => '$5'}, extract_location('$2')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name ImplementsInterfaces DirectivesConst FieldsDefinition : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5', 'fields' => '$6'}, extract_location('$2')).

InterfaceTypeExtension -> 'extend' 'interface' Name DirectivesConst : parse(#{'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$1')).
InterfaceTypeExtension -> 'extend' 'interface' Name FieldsDefinition : parse(#{'name' => extract_binary('$3'), 'fields' => '$4'}, extract_location('$1')).
InterfaceTypeExtension -> 'extend' 'interface' Name ImplementsInterfaces : parse(#{'name' => extract_binary('$3'), 'implements' => '$4'}, extract_location('$1')).
InterfaceTypeExtension -> 'extend' 'interface' Name DirectivesConst FieldsDefinition : parse(#{'name' => extract_binary('$3'), 'directives' => '$4', 'fields' => '$5'}, extract_location('$1')).
InterfaceTypeExtension -> 'extend' 'interface' Name ImplementsInterfaces DirectivesConst : parse(#{'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5'}, extract_location('$1')).
InterfaceTypeExtension -> 'extend' 'interface' Name ImplementsInterfaces FieldsDefinition : parse(#{'name' => extract_binary('$3'), 'implements' => '$4', 'fields' => '$5'}, extract_location('$1')).
InterfaceTypeExtension -> 'extend' 'interface' Name ImplementsInterfaces DirectivesConst FieldsDefinition : parse(#{'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5', 'fields' => '$6'}, extract_location('$1')).

NameWithoutOn -> 'name' : '$1'.
NameWithoutOn -> 'query' : '$1'.
NameWithoutOn -> 'mutation' : '$1'.
NameWithoutOn -> 'subscription' : '$1'.
NameWithoutOn -> 'fragment' : '$1'.
NameWithoutOn -> 'type' : '$1'.
NameWithoutOn -> 'implements' : '$1'.
NameWithoutOn -> 'interface' : '$1'.
NameWithoutOn -> 'union' : '$1'.
NameWithoutOn -> 'scalar' : '$1'.
NameWithoutOn -> 'schema' : '$1'.
NameWithoutOn -> 'enum' : '$1'.
NameWithoutOn -> 'input' : '$1'.
NameWithoutOn -> 'extend' : '$1'.
NameWithoutOn -> 'directive' : '$1'.
Name -> NameWithoutOn : '$1'.
Name -> 'on' : '$1'.

ObjectTypeDefinition -> 'type' Name : parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
ObjectTypeDefinition -> 'type' Name ImplementsInterfaces : parse(#{'name' => extract_binary('$2'), 'implements' => '$3'}, extract_location('$1')).
ObjectTypeDefinition -> 'type' Name DirectivesConst : parse(#{'name' => extract_binary('$2'), 'directives' => '$3'}, extract_location('$1')).
ObjectTypeDefinition -> 'type' Name FieldsDefinition : parse(#{'name' => extract_binary('$2'), 'fields' => '$3'}, extract_location('$1')).
ObjectTypeDefinition -> 'type' Name ImplementsInterfaces DirectivesConst : parse(#{'name' => extract_binary('$2'), 'implements' => '$3', 'directives' => '$4'}, extract_location('$1')).
ObjectTypeDefinition -> 'type' Name ImplementsInterfaces FieldsDefinition : parse(#{'name' => extract_binary('$2'), 'implements' => '$3', 'fields' => '$4'}, extract_location('$1')).
ObjectTypeDefinition -> 'type' Name DirectivesConst FieldsDefinition : parse(#{'name' => extract_binary('$2'), 'directives' => '$3', 'fields' => '$4'}, extract_location('$1')).
ObjectTypeDefinition -> 'type' Name ImplementsInterfaces DirectivesConst FieldsDefinition : parse(#{'name' => extract_binary('$2'), 'implements' => '$3', 'directives' => '$4', 'fields' => '$5'}, extract_location('$1')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name : parse(#{'description' => '$1', 'name' => extract_binary('$3')}, extract_location('$2')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name ImplementsInterfaces : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4'}, extract_location('$2')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name DirectivesConst : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$2')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name FieldsDefinition : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'fields' => '$4'}, extract_location('$2')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name ImplementsInterfaces DirectivesConst : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5'}, extract_location('$2')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name ImplementsInterfaces FieldsDefinition : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4', 'fields' => '$5'}, extract_location('$2')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name DirectivesConst FieldsDefinition : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4', 'fields' => '$5'}, extract_location('$2')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name ImplementsInterfaces DirectivesConst FieldsDefinition : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5', 'fields' => '$6'}, extract_location('$2')).

ObjectTypeExtension -> 'extend' 'type' Name DirectivesConst : parse(#{'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$1')).
ObjectTypeExtension -> 'extend' 'type' Name FieldsDefinition : parse(#{'name' => extract_binary('$3'), 'fields' => '$4'}, extract_location('$1')).
ObjectTypeExtension -> 'extend' 'type' Name ImplementsInterfaces : parse(#{'name' => extract_binary('$3'), 'implements' => '$4'}, extract_location('$1')).
ObjectTypeExtension -> 'extend' 'type' Name DirectivesConst FieldsDefinition : parse(#{'name' => extract_binary('$3'), 'directives' => '$4', 'fields' => '$5'}, extract_location('$1')).
ObjectTypeExtension -> 'extend' 'type' Name ImplementsInterfaces DirectivesConst : parse(#{'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5'}, extract_location('$1')).
ObjectTypeExtension -> 'extend' 'type' Name ImplementsInterfaces FieldsDefinition : parse(#{'name' => extract_binary('$3'), 'implements' => '$4', 'fields' => '$5'}, extract_location('$1')).
ObjectTypeExtension -> 'extend' 'type' Name ImplementsInterfaces DirectivesConst FieldsDefinition : parse(#{'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5', 'fields' => '$6'}, extract_location('$1')).

OperationDefinition -> SelectionSet : parse(#{'operation' => 'query', 'selection_set' => '$1', 'shorthand' => true}, extract_location('$1')).
OperationDefinition -> OperationType SelectionSet : parse(#{'operation' => extract_atom('$1'), 'selection_set' => '$2'}, extract_location('$1')).
OperationDefinition -> OperationType VariablesDefinition SelectionSet : parse(#{'operation' => extract_atom('$1'), 'variables_definition' => '$2', 'selection_set' => '$3'}, extract_location('$1')).
OperationDefinition -> OperationType Directives SelectionSet : parse(#{'operation' => extract_atom('$1'), 'directives' => '$2', 'selection_set' => '$3'}, extract_location('$1')).
OperationDefinition -> OperationType VariablesDefinition Directives SelectionSet : parse(#{'operation' => extract_atom('$1'), 'variables_definition' => '$2', 'directives' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
OperationDefinition -> OperationType Name SelectionSet : parse(#{'operation' => extract_atom('$1'), 'name' => extract_binary('$2'), 'selection_set' => '$3'}, extract_location('$1')).
OperationDefinition -> OperationType Name VariablesDefinition SelectionSet : parse(#{'operation' => extract_atom('$1'), 'name' => extract_binary('$2'), 'variables_definition' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
OperationDefinition -> OperationType Name Directives SelectionSet : parse(#{'operation' => extract_atom('$1'), 'name' => extract_binary('$2'), 'directives' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
OperationDefinition -> OperationType Name VariablesDefinition Directives SelectionSet : parse(#{'operation' => extract_atom('$1'), 'name' => extract_binary('$2'), 'variables_definition' => '$3', 'directives' => '$4', 'selection_set' => '$5'}, extract_location('$1')).

OperationType -> 'query' : '$1'.
OperationType -> 'mutation' : '$1'.
OperationType -> 'subscription' : '$1'.

RootOperationTypesDefinition -> '{' '}' : parse(#{'operations' => []}, extract_location('$1')).
RootOperationTypesDefinition -> '{' RootOperationTypeDefinitionList '}' : parse(#{'operations' => '$2'}, extract_location('$1')).
RootOperationTypeDefinitionList -> RootOperationTypeDefinition : ['$1'].
RootOperationTypeDefinitionList -> RootOperationTypeDefinition RootOperationTypeDefinitionList : ['$1'|'$2'].
RootOperationTypeDefinition -> OperationType ':' NamedType : parse(#{'operation_type' => extract_atom('$1'), 'named_type' => '$3'}, extract_location('$1')).

ScalarTypeDefinition -> 'scalar' Name : parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
ScalarTypeDefinition -> 'scalar' Name DirectivesConst : parse(#{'name' => extract_binary('$2'), 'directives' => '$3'}, extract_location('$1')).
ScalarTypeDefinition -> DescriptionDefinition 'scalar' Name : parse(#{'description' => '$1', 'name' => extract_binary('$3')}, extract_location('$2')).
ScalarTypeDefinition -> DescriptionDefinition 'scalar' Name DirectivesConst : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$2')).

ScalarTypeExtension -> 'extend' 'scalar' Name DirectivesConst : parse(#{'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$1')).

SchemaDefinition -> 'schema' DirectivesConst : parse(#{'directives' => '$2'}, extract_location('$1')).
SchemaDefinition -> 'schema' RootOperationTypesDefinition : parse(#{'operations' => '$2'}, extract_location('$1')).
SchemaDefinition -> 'schema' DirectivesConst RootOperationTypesDefinition : parse(#{'directives' => '$2', 'operations' => '$3'}, extract_location('$1')).
SchemaDefinition -> DescriptionDefinition 'schema' DirectivesConst : parse(#{'description' => '$1', 'directives' => '$3'}, extract_location('$2')).
SchemaDefinition -> DescriptionDefinition 'schema' RootOperationTypesDefinition : parse(#{'description' => '$1', 'operations' => '$3'}, extract_location('$2')).
SchemaDefinition -> DescriptionDefinition 'schema' DirectivesConst RootOperationTypesDefinition : parse(#{'description' => '$1', 'directives' => '$3', 'operations' => '$4'}, extract_location('$2')).

SchemaExtension -> 'extend' 'schema' DirectivesConst : parse(#{'directives' => '$3'}, extract_location('$1')).
SchemaExtension -> 'extend' 'schema' RootOperationTypesDefinition : parse(#{'operations' => '$3'}, extract_location('$1')).
SchemaExtension -> 'extend' 'schema' DirectivesConst RootOperationTypesDefinition : parse(#{'directives' => '$3', 'operations' => '$4'}, extract_location('$1')).

SelectionSet -> '{' SelectionList '}' : parse(#{'selections' => '$2'}, extract_location('$1')).
SelectionList -> Selection : ['$1'].
SelectionList -> Selection SelectionList : ['$1'|'$2'].
Selection -> Field : field('$1', extract_location('$1')).
Selection -> FragmentSpread : fragment_spread('$1', extract_location('$1')).
Selection -> InlineFragment : inline_fragment('$1', extract_location('$1')).

Type -> NamedType : named_type('$1', extract_location('$1')).
Type -> ListType : list_type('$1', extract_location('$1')).
Type -> NonNullType : non_null_type('$1', extract_location('$1')).
NamedType -> Name : parse(#{'name' => extract_binary('$1')}, extract_location('$1')).
ListType -> '[' Type ']' : parse(#{'type' => '$2'}, extract_location('$1')).
NonNullType -> NamedType '!' : parse(#{'type' => '$1'}, extract_location('$1')).
NonNullType -> ListType '!' : parse(#{'type' => '$1'}, extract_location('$1')).

TypeDefinition -> ScalarTypeDefinition : scalar_type_definition('$1', extract_location('$1')).
TypeDefinition -> ObjectTypeDefinition : object_type_definition('$1', extract_location('$1')).
TypeDefinition -> InterfaceTypeDefinition : interface_type_definition('$1', extract_location('$1')).
TypeDefinition -> UnionTypeDefinition : union_type_definition('$1', extract_location('$1')).
TypeDefinition -> EnumTypeDefinition : enum_type_definition('$1', extract_location('$1')).
TypeDefinition -> InputObjectTypeDefinition : input_object_type_definition('$1', extract_location('$1')).

TypeExtension -> ScalarTypeExtension : scalar_type_extension('$1', extract_location('$1')).
TypeExtension -> ObjectTypeExtension : object_type_extension('$1', extract_location('$1')).
TypeExtension -> InterfaceTypeExtension : interface_type_extension('$1', extract_location('$1')).
TypeExtension -> UnionTypeExtension : union_type_extension('$1', extract_location('$1')).
TypeExtension -> EnumTypeExtension : enum_type_extension('$1', extract_location('$1')).
TypeExtension -> InputObjectTypeExtension : input_object_type_extension('$1', extract_location('$1')).

TypeSystemDefinition -> SchemaDefinition : schema_definition('$1', extract_location('$1')).
TypeSystemDefinition -> TypeDefinition : type_definition('$1', extract_location('$1')).
TypeSystemDefinition -> DirectiveDefinition : directive_definition('$1', extract_location('$1')).

TypeSystemExtension -> SchemaExtension : schema_extension('$1', extract_location('$1')).
TypeSystemExtension -> TypeExtension : type_extension('$1', extract_location('$1')).

UnionTypeDefinition -> 'union' Name : parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
UnionTypeDefinition -> 'union' Name DirectivesConst : parse(#{'name' => extract_binary('$2'), 'directives' => '$3'}, extract_location('$1')).
UnionTypeDefinition -> 'union' Name '=' UnionMemberTypes : parse(#{'name' => extract_binary('$2'), 'types' => '$4'}, extract_location('$1')).
UnionTypeDefinition -> 'union' Name DirectivesConst '=' UnionMemberTypes : parse(#{'name' => extract_binary('$2'), 'directives' => '$3', 'types' => '$5'}, extract_location('$1')).
UnionTypeDefinition -> DescriptionDefinition 'union' Name : parse(#{'description' => '$1', 'name' => extract_binary('$3')}, extract_location('$2')).
UnionTypeDefinition -> DescriptionDefinition 'union' Name DirectivesConst : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$2')).
UnionTypeDefinition -> DescriptionDefinition 'union' Name '=' UnionMemberTypes : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'types' => '$5'}, extract_location('$2')).
UnionTypeDefinition -> DescriptionDefinition 'union' Name DirectivesConst '=' UnionMemberTypes : parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4', 'types' => '$6'}, extract_location('$2')).
UnionMemberTypes -> NamedType : ['$1'].
UnionMemberTypes -> NamedType '|' UnionMemberTypes : ['$1'|'$3'].
UnionMemberTypes -> '|' NamedType '|' UnionMemberTypes : ['$2'|'$4'].

UnionTypeExtension -> 'extend' 'union' Name DirectivesConst : parse(#{'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$1')).
UnionTypeExtension -> 'extend' 'union' Name '=' UnionMemberTypes : parse(#{'name' => extract_binary('$3'), 'types' => '$5'}, extract_location('$1')).
UnionTypeExtension -> 'extend' 'union' Name DirectivesConst '=' UnionMemberTypes : parse(#{'name' => extract_binary('$3'), 'directives' => '$4', 'types' => '$6'}, extract_location('$1')).

Value -> Variable : variable('$1', extract_location('$1')).
Value -> int_value : int(extract_integer('$1'), extract_location('$1')).
Value -> float_value : float(extract_float('$1'), extract_location('$1')).
Value -> block_string_value : string(extract_quoted_block_string_token('$1'), extract_location('$1')).
Value -> string_value : string(extract_quoted_string_token('$1'), extract_location('$1')).
Value -> boolean_value : boolean(extract_boolean('$1'), extract_location('$1')).
Value -> null : null(extract_location('$1')).
Value -> EnumValue : enum('$1', extract_location('$1')).
Value -> ListValue : list('$1', extract_location('$1')).
Value -> ObjectValue : object('$1', extract_location('$1')).
EnumValue -> Name : parse(#{'name' => extract_binary('$1')}, extract_location('$1')).
ListValue -> '[' ']' : parse(#{'list' => []}, extract_location('$1')).
ListValue -> '[' ValueList ']' : parse(#{'list' => '$2'}, extract_location('$1')).
ValueList -> Value : ['$1'].
ValueList -> Value ValueList : ['$1'|'$2'].
ObjectValue -> '{' '}' : parse(#{'fields' => []}, extract_location('$1')).
ObjectValue -> '{' ObjectFieldList '}' : parse(#{'fields' => '$2'}, extract_location('$1')).
ObjectFieldList -> ObjectField : ['$1'].
ObjectFieldList -> ObjectField ObjectFieldList : ['$1'|'$2'].
ObjectField -> Name ':' Value : parse(#{'name' => extract_binary('$1'), 'value' => '$3'}, extract_location('$1')).

ValueConst -> int_value : int(extract_integer('$1'), extract_location('$1')).
ValueConst -> float_value : float(extract_float('$1'), extract_location('$1')).
ValueConst -> block_string_value : string(extract_quoted_block_string_token('$1'), extract_location('$1')).
ValueConst -> string_value : string(extract_quoted_string_token('$1'), extract_location('$1')).
ValueConst -> boolean_value : boolean(extract_boolean('$1'), extract_location('$1')).
ValueConst -> null : null(extract_location('$1')).
ValueConst -> EnumValue : enum('$1', extract_location('$1')).
ValueConst -> ListValueConst : list('$1', extract_location('$1')).
ValueConst -> ObjectValueConst : object('$1', extract_location('$1')).
ListValueConst -> '[' ']' : parse(#{'list' => []}, extract_location('$1')).
ListValueConst -> '[' ValueConstList ']' : parse(#{'list' => '$2'}, extract_location('$1')).
ValueConstList -> ValueConst : ['$1'].
ValueConstList -> ValueConst ValueConstList : ['$1'|'$2'].
ObjectValueConst -> '{' '}' : parse(#{'fields' => []}, extract_location('$1')).
ObjectValueConst -> '{' ObjectFieldConstList '}' : parse(#{'fields' => '$2'}, extract_location('$1')).
ObjectFieldConstList -> ObjectFieldConst : ['$1'].
ObjectFieldConstList -> ObjectFieldConst ObjectFieldConstList : ['$1'|'$2'].
ObjectFieldConst -> Name ':' ValueConst : parse(#{'name' => extract_binary('$1'), 'value' => '$3'}, extract_location('$1')).

VariablesDefinition -> '(' VariableDefinitionList ')' : parse(#{'variables' => '$2'}, extract_location('$1')).
VariableDefinitionList -> VariableDefinition : ['$1'].
VariableDefinitionList -> VariableDefinition VariableDefinitionList : ['$1'|'$2'].
VariableDefinition -> Variable ':' Type : parse(#{'variable' => '$1', 'type' => '$3'}, extract_location('$1')).
VariableDefinition -> Variable ':' Type DirectivesConst: parse(#{'variable' => '$1', 'type' => '$3', 'directives' => '$4'}, extract_location('$1')).
VariableDefinition -> Variable ':' Type DefaultValue : parse(#{'variable' => '$1', 'type' => '$3', 'default_value' => '$4'}, extract_location('$1')).
VariableDefinition -> Variable ':' Type DefaultValue DirectivesConst : parse(#{'variable' => '$1', 'type' => '$3', 'default_value' => '$4', 'directives' => '$5'}, extract_location('$1')).
Variable -> '$' NameWithoutOn : parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
Variable -> '$' 'on' : parse(#{'name' => extract_binary('$2')}, extract_location('$1')).

Expect 35.

Erlang code.

-ignore_xref([return_error/2]).

parse(X, Y) -> {X, Y}.

executable_definition(X, Y) -> {X, Y}.

executable_directive_location(X, Y) -> {X, Y}.

type_system_definition(X, Y) -> {X, Y}.

type_system_extension(X, Y) -> {X, Y}.

object(X, Y) -> {X, Y}.

list(X, Y) -> {X, Y}.

enum(X, Y) -> {X, Y}.

boolean(X, Y) -> {X, Y}.

string(X, Y) -> {X, Y}.

float(X, Y) -> {X, Y}.

int(X, Y) -> {X, Y}.

variable(X, Y) -> {X, Y}.

type_extension(X, Y) -> {X, Y}.

schema_extension(X, Y) -> {X, Y}.

directive_definition(X, Y) -> {X, Y}.

type_definition(X, Y) -> {X, Y}.

schema_definition(X, Y) -> {X, Y}.

input_object_type_extension(X, Y) -> {X, Y}.

enum_type_extension(X, Y) -> {X, Y}.

union_type_extension(X, Y) -> {X, Y}.

interface_type_extension(X, Y) -> {X, Y}.

object_type_extension(X, Y) -> {X, Y}.

scalar_type_extension(X, Y) -> {X, Y}.

input_object_type_definition(X, Y) -> {X, Y}.

enum_type_definition(X, Y) -> {X, Y}.

union_type_definition(X, Y) -> {X, Y}.

interface_type_definition(X, Y) -> {X, Y}.

object_type_definition(X, Y) -> {X, Y}.

scalar_type_definition(X, Y) -> {X, Y}.

non_null_type(X, Y) -> {X, Y}.

list_type(X, Y) -> {X, Y}.

named_type(X, Y) -> {X, Y}.

inline_fragment(X, Y) -> {X, Y}.

fragment_spread(X, Y) -> {X, Y}.

field(X, Y) -> {X, Y}.

fragment_definition(X, Y) -> {X, Y}.

operation_definition(X, Y) -> {X, Y}.

type_system_directive_location(X, Y) -> {X, Y}.

unicode_binary(X) -> X.  %TODO

name_from_string(X) -> X.  %TODO

null(X) -> X.  %TODO

% Line-Level Utilities

-dialyzer({nowarn_function, extract_location/1}).
extract_location({_Token, {Line, Column}}) ->
  erl_anno:new({Line, Column});
extract_location({_Token, {Line, Column}, _Value}) ->
  erl_anno:new({Line, Column});
extract_location(Record) when is_tuple(Record) andalso tuple_size(Record) >= 2 andalso is_atom(element(1, Record)) andalso is_tuple(element(2, Record)) andalso tuple_size(element(2, Record)) =:= 2 ->
  {Line, Column} = element(2, Record),
  erl_anno:new({Line, Column});
extract_location([Record | _]) when is_tuple(Record) ->
  extract_location(Record).

% Value-level Utilities

extract_atom({Value, _Loc}) when is_atom(Value) ->
  Value;
extract_atom({executable_directive_location, _Loc, Value}) ->
  name_from_string(Value);
extract_atom({type_system_directive_location, _Loc, Value}) ->
  name_from_string(Value);
extract_atom({Token, Loc, Value}) ->
  return_error(Loc, lists:flatten(io_lib:format("extract_atom/1 not supported for Token=~0tp, Value=~0tp", [Token, Value]))).

extract_binary(Value) when is_binary(Value) ->
  Value;
extract_binary({Token, _Loc}) when is_atom(Token) ->
  erlang:atom_to_binary(Token, utf8);
extract_binary({_Token, _Loc, Value}) ->
  unicode_binary(Value).

% String

extract_quoted_string_token({_Token, _Loc, Value}) ->
  unicode_binary(lists:sublist(Value, 2, length(Value) - 2)).

% Block String

extract_quoted_block_string_token({_Token, _Loc, Value}) ->
  unicode_binary(process_block_string(lists:sublist(Value, 4, length(Value) - 6))).

-spec process_block_string(string()) -> string().
process_block_string(Escaped) ->
  process_block_string(Escaped, []).

-spec process_block_string(string(), string()) -> string().
process_block_string([], Acc) ->
  block_string_value(lists:reverse(Acc));
process_block_string([$\r, $\n | T], Acc) -> process_block_string(T, [$\n | Acc]);
process_block_string([$\\, $", $", $" | T], Acc) -> process_block_string(T, [$", $", $"] ++ Acc);
process_block_string([H | T], Acc) -> process_block_string(T, [H | Acc]).

-spec block_string_value(string()) -> string().
block_string_value(Value) ->
  [FirstLine | Rest] = string:split(Value, "\n", all),
  Prefix = indentation_prefix(common_indent(Rest)),
  UnindentedLines = unindent(Rest, Prefix),
  Lines = trim_blank_lines([FirstLine | UnindentedLines]),
  string:join(Lines, "\n").

-spec trim_blank_lines([string()]) -> [string()].
trim_blank_lines(Lines) ->
  trim_blank_lines(trim_blank_lines(Lines, leading), trailing).

-spec trim_blank_lines([string()], leading | trailing) -> [string()].
trim_blank_lines(Lines, leading) ->
  lists:dropwhile(fun is_blank/1, Lines);
trim_blank_lines(Lines, trailing) ->
  lists:reverse(trim_blank_lines(lists:reverse(Lines), leading)).

-spec indentation_prefix(non_neg_integer()) -> string().
indentation_prefix(Indent) ->
  lists:map(fun(_) -> 32 end, lists:seq(1, Indent)).

-spec unindent([string()], string()) -> [string()].
unindent(Lines, Prefix) ->
  unindent(Lines, Prefix, []).

-spec unindent([string()], string(), [string()]) -> [string()].
unindent([], _Prefix, Result) ->
  lists:reverse(Result);
unindent([H | T], Prefix, Result) ->
  Processed = prefix(H, Prefix),
  unindent(T, Prefix, [Processed | Result]).

-spec prefix(string(), string()) -> string().
prefix(Line, []) ->
  Line;
prefix(Line, Prefix) ->
  Prefixed = lists:prefix(Prefix, Line),
  if
    Prefixed ->
      string:substr(Line, length(Prefix) + 1);
    true ->
      Line
  end.

-spec common_indent([string()]) -> non_neg_integer().
common_indent(Lines) ->
  case common_indent(Lines, noindent) of
    noindent ->
      0;
    Indent ->
      Indent
  end.

-spec common_indent([string()], noindent | non_neg_integer()) -> noindent | non_neg_integer().
common_indent([], Indent) ->
    Indent;
common_indent([H | T], Indent) ->
  CurrentIndent = leading_whitespace(H),
  if
    (CurrentIndent < length(H)) and ((Indent == noindent) or (CurrentIndent < Indent)) ->
      common_indent(T, CurrentIndent);
    true ->
      common_indent(T, Indent)
  end.

-spec leading_whitespace(string()) -> non_neg_integer().
leading_whitespace(BlockStringValue) ->
  leading_whitespace(BlockStringValue, 0).

-spec leading_whitespace(string(), non_neg_integer()) -> non_neg_integer().
leading_whitespace([], N) ->
  N;
leading_whitespace([32 | T], N) ->
  leading_whitespace(T, N + 1);
leading_whitespace([$\t | T], N) ->
  leading_whitespace(T, N + 1);
leading_whitespace([_H | _T], N) ->
  N.

-spec is_blank(string()) -> boolean().
is_blank(BlockStringValue) ->
    leading_whitespace(BlockStringValue) == length(BlockStringValue).

% Integer

extract_integer({int_value, _Loc, Value}) when is_integer(Value) ->
  Value.

% Float

extract_float({float_value, _Loc, Value}) when is_float(Value) ->
  Value.

% Boolean

extract_boolean({boolean_value, _Loc, Value}) when is_boolean(Value) ->
  Value.
