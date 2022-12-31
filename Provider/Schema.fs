namespace JsonSchema

open System.Collections.Generic

type KeywordCollection = IReadOnlyCollection<Json.Schema.IJsonSchemaKeyword>

module Schema =

let pickKeyword<'T when 'T :> Json.Schema.IJsonSchemaKeyword> (keywords: KeywordCollection) : 'T option =
    let picked =
        keywords
        |> Seq.choose (function
            | :? 'T as t -> Some t
            | _ -> None)
        |> Seq.toArray

    // Error if we see more than 1 of the same type of keyword, mostly for sanity
    if picked.Length > 1 then
        failwithf "Found more than one keyword of type %s" (typeof<'T>.FullName)

    if picked.Length = 1 then Some picked[0] else None