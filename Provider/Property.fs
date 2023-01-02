module JsonSchema.Property

open Pulumi.Provider
open System.Text.Json
open System.Text.Json.Nodes

type NumericValidation = { Max: decimal option }

// JSON Schema => Schema
// Schema => Type
// Schema * JSON Instance => PropertyValue
// Schema * Property Value => JSON Instance

// anyOf = union of subschemas, intersection with root schema
// allOf = intersection of subschemas, intersection with root schema
// oneOf = disjoint union of subschemas, intersection with root schema
// not = complement of subschema, intersection with root schema
// null = union with root schema
// integer = union with root schema
// etc...

// The complement of True is False
// The complement of False is True
// The complement of (Complement[X]) is X
// The complement of (Union [X...]) is (Intersection [Complement[X]...])
// The complement of (Intersection [X...]) is (Union [Complement[X]...])
// The complement of (DU [X...]) is:
// DU[X,Y] = (X|Y)&~(X&Y) = (X|Y)&(~X|~Y) therefore
// ~DU[X,Y] = ~(X|Y)|~(~X|~Y) = (~X&~Y)|(X&Y)
// The complement of anything else is just Complement[X]
// The type of Complement[X] is Any (generally, we might be able to do better in some cases)

// The disjoint union of X and X is X
// The disjoint union of True and X is True
// The disjoint union of X and False is X
// The disjoint union of anything else is just DU[X...]
// DU[X] = X
// The type of DU[X...] is Object{Choice1ofX: X...}

// The union of X and X is X
// The union of X and True is True
// The union of X and False is X
// The union of two unions is U[AXB => U(a, b)], that is take the cross-product try to unify each thing, take the set of unique results as a new union
// The union of anything else is just U[X...]
// U[X] = X
// The type of U[X...] is Object{Option1ofX: X...}

// The intersection of X and X is X
// The intersection of X and True is X
// The intersection of X and False is False
// The intersection of two different types is False
// I[X] = X
// The type of I[X...] is Object{Required1ofX: X...}


// Older notes
// The union of two different types is that type, but that doesn't track things like maxLength well
// The intersection of two objects is just a new object with all the valid fields, constrained maximally (or void if this can't be done)
// The intersection of two arrays is an array of the intersection if the inner type, same for maps. That might be void, it's still an array/map.

// This still feels like a good idea
// What if Validation works on JSON layout, but the Type converts from Pulumi layout to JSON layout and vice versa
// e.g. A Tuple type goes from {item1: x} to [x] and vice versa, a string type is a noop.

// So you then have
// JSON Schema => Schema
// Schema => Type
// Type => PropertyValue -> PropertyValue * PropertyValue -> PropertyValue

// A schema constrains the set of valid values
// The set of all valid values (after constraints) is a type
// The fun is mapping that type to a pulumi type

// The trickyness with allOf is I'm not sure if it should be seen as an embeding or intersection...
// I think if an intersection is disjoint (yeh that sounds weird, what I mean is disjoint vis-a-vis listed properties) then it should be embeded, else it should be intersected.
// That is given some thing like {a: int, allOf: [{ b: int }]} you'd want {b:int} to be an embeded field (naming is tricky, but titles or allOf0 at the least.
// But if you had {a:string, allOf: [{a: {string, format:uri}}]} you'd just want one root {a:string} object.

// Each validation below is responsible for all the assertions of one type (e.g. string)

type BooleanValidation = { Enum: bool Set; Const: bool option }

// These are the annotations that can be returned by applicators
type Annotations = { PrefixItems: int }

// These are applicators to apply subschemas to a root schema
type Applicators =
    { Ref: Schema option
      OneOf: Schema list
      AllOf: Schema list
      AnyOf: Schema list
      Not: Schema option
      If: (Schema * Schema option * Schema option) option
      DependentSchemas: Map<string, Schema> }

    static member None =
        { Ref = None
          OneOf = []
          AllOf = []
          AnyOf = []
          Not = None
          If = None
          DependentSchemas = Map.empty }

// These are the assertions for each JSON type. If they are None/False that type is not valid.
and Assertions =
    { Null: bool
      Boolean: BooleanValidation option }

    static member False = { Null = false; Boolean = None }

    static member True =
        { Null = true
          Boolean = Some { Enum = Set.empty; Const = None } }

and Schema =
    { Location: Json.Pointer.JsonPointer

      Title: string option
      Description: string option

      Applicators: Applicators
      Assertions: Assertions }

module Schema =

    let parse (path: Json.Pointer.JsonPointer) (schema: Json.Schema.JsonSchema) =
        match schema.BoolValue |> Option.ofNullable with
        | Some false ->
            { Location = path

              Title = None
              Description = None

              Applicators = Applicators.None
              Assertions = Assertions.False }
        | Some true ->
            { Location = path

              Title = None
              Description = None

              Applicators = Applicators.None
              Assertions = Assertions.True }
        | None ->
            let keywords = schema.Keywords

            let applicators = Applicators.None
            let assertions = Assertions.True

            { Location = path

              Title =
                  Schema.pickKeyword<Json.Schema.TitleKeyword> keywords
                  |> Option.map (fun kw -> kw.Value)
              Description =
                Schema.pickKeyword<Json.Schema.DescriptionKeyword> keywords
                |> Option.map (fun kw -> kw.Value)

              Applicators = applicators
              Assertions = assertions }

    let read (schema: Schema) (path: Json.Pointer.JsonPointer) (element: System.Text.Json.JsonElement) =

        // Handle type
        failwith ""
        
// JSet?
// How does unevaluatedItems/Properties work with sets?
[<RequireQualifiedAccess>]
type SchemaType =
    | Void
    | Null
    | Boolean of BooleanValidation
    | DisjointUnion of Set<SchemaType>
    | Union of Set<SchemaType>
    | Intersection of Set<SchemaType>
    | Complement of SchemaType

module Type =

    let any = 
        SchemaType.Union (Set.ofList [
            SchemaType.Null
            SchemaType.Boolean { Enum = Set.empty; Const = None }
        ])

    type EmptyOrExactlyOne<'T> =
        | Empty
        | ExactlyOne of 'T
        | MoreThanOne 

    let tryEmptyOrExactlyOne (set : Set<'T>) = 
        if set.Count = 0 then Empty
        else 
            match Seq.tryExactlyOne set with
            | Some one -> ExactlyOne one
            | None -> MoreThanOne            

    let rec union (a : SchemaType) (b :SchemaType) : SchemaType =
        match a, b with 
        | a, b when a = b -> a
        | a, SchemaType.Complement b when a = b -> any
        | a, _ when a = any -> any
        | _, b when b = any -> any
        | SchemaType.Void, b -> b
        | a, SchemaType.Void -> a
        | SchemaType.Union a, SchemaType.Union b ->      
            // Given A B C if we want the smallest union we can
            // union the cross product and then intersect i.e.
            // (A|B) & (A|C) & (B|C)
            // Some of those unions might reduce
            Set.union a b
            |> fun u -> Seq.allPairs u u 
            |> Seq.map (fun (a, b) -> union a b)
            |> Seq.fold intersect any
        | SchemaType.Union a, b -> union (SchemaType.Union a) (SchemaType.Union (Set.singleton b))
        | a, SchemaType.Union b -> union (SchemaType.Union (Set.singleton a)) (SchemaType.Union b) 
        | _, _ -> SchemaType.Union (Set.ofList [a; b])

    and intersect (a : SchemaType) (b :SchemaType) : SchemaType =
        match a, b with 
        | a, b when a = b -> a
        | a, SchemaType.Complement b when a = b -> SchemaType.Void
        | a, b when a = any -> b
        | a, b when b = any -> a
        | SchemaType.Void, _ -> SchemaType.Void
        | _, SchemaType.Void -> SchemaType.Void
        | SchemaType.Intersection a, SchemaType.Intersection b ->
            let i = Set.intersect a b
            match tryEmptyOrExactlyOne i with
            | Empty -> SchemaType.Void
            | ExactlyOne x -> x
            | MoreThanOne ->
                // Given A B C if we want the smallest intersect we can
                // Intersect the cross product and then union i.e.
                // (A&B) | (A&C) | (B&C)
                // Some of those intersects might reduce
                Seq.allPairs i i
                |> Seq.map (fun (a, b) -> intersect a b)
                |> Seq.fold union SchemaType.Void
        | SchemaType.Intersection a, b -> intersect (SchemaType.Intersection a) (SchemaType.Intersection (Set.singleton b))
        | a, SchemaType.Intersection b -> intersect (SchemaType.Intersection (Set.singleton a)) (SchemaType.Intersection b) 
        | SchemaType.Union a, SchemaType.Union b -> 
            let u = Set.intersect a b 
            match tryEmptyOrExactlyOne u with
            | Empty -> SchemaType.Void
            | ExactlyOne x -> x
            | MoreThanOne -> SchemaType.Union u
        | SchemaType.Boolean _, SchemaType.Boolean _ -> SchemaType.Intersection (Set.ofList [a; b])
        | _, _ -> SchemaType.Void

    let rec complement (a : SchemaType) : SchemaType =
        match a with
        | SchemaType.Void -> any
        | SchemaType.Null -> SchemaType.Boolean { Enum = Set.empty; Const = None }
        | SchemaType.Complement c -> c
        | SchemaType.Union u ->
            Seq.fold (fun i x -> intersect i (complement x)) SchemaType.Void u
        | SchemaType.Intersection i ->
            Seq.fold (fun u x -> union u (complement x)) SchemaType.Void i
        | x -> SchemaType.Complement x

    let rec fromSchema (schema : Schema) : SchemaType =    
        let applicators = 
            [
                Option.map (fromSchema >> complement) schema.Applicators.Not
            ]
            |> Seq.choose id
            |> Seq.fold intersect SchemaType.Void

        let assertions =
            [
                if schema.Assertions.Null then Some SchemaType.Null else None
                Option.map SchemaType.Boolean schema.Assertions.Boolean
            ]
            |> Seq.choose id
            |> Seq.fold union SchemaType.Void

        intersect applicators assertions

[<RequireQualifiedAccess>]
type Conversion = 
    | Void of Schema
    | Null of Schema
    | Boolean of Schema
    | Tuple of Conversion list * Conversion option * Schema

module Conversion = 

    let fromSchema (schema : Schema) =
        let typ = Type.fromSchema schema

        match typ with 
        | SchemaType.Void -> Conversion.Void schema
        | SchemaType.Null -> Conversion.Null schema
        | SchemaType.Boolean _ -> Conversion.Boolean schema

    let rec read (conversion : Conversion) (path: Json.Pointer.JsonPointer) (element: System.Text.Json.JsonElement) =
        match conversion with 
        | Conversion.Top schema -> Schema.read schema path element
        | Conversion.Null schema -> Schema.read schema path element
        | Conversion.Boolean schema -> Schema.read schema path element


        | Conversion.Tuple (prefix, items, schema) -> 
            


