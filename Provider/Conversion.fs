/// Given a JSON schema generate a Pulumi schema, and a way to read/write from that pulumi type to the expected json.
module JsonSchema.Conversion

open System.Text.Json
open System.Text.Json.Nodes

type StringConversion =
    { MinLength: uint option
      MaxLength: uint option
      Pattern: string option }

    static member FromKeywords(keywords: KeywordCollection) =
        { MinLength =
            Schema.pickKeyword<Json.Schema.MinLengthKeyword> keywords
            |> Option.map (fun kw -> kw.Value)
          MaxLength =
            Schema.pickKeyword<Json.Schema.MaxLengthKeyword> keywords
            |> Option.map (fun kw -> kw.Value)
          Pattern =
            Schema.pickKeyword<Json.Schema.PatternKeyword> keywords
            |> Option.map (fun kw -> kw.Value.ToString()) }

    member private this.Validate(value: string) =
        let minCheck =
            match this.MinLength with
            | Some l when uint value.Length < l ->
                sprintf "Value is not longer than or equal to %d characters" l |> Some
            | _ -> None

        let maxCheck =
            match this.MaxLength with
            | Some l when uint value.Length > l ->
                sprintf "Value is not shorter than or equal to %d characters" l |> Some
            | _ -> None

        let patternCheck =
            match this.Pattern with
            | Some pattern ->
                let re =
                    System.Text.RegularExpressions.Regex(
                        pattern,
                        System.Text.RegularExpressions.RegexOptions.ECMAScript
                    )

                if re.IsMatch value |> not then
                    Some "The string value was not a match for the indicated regular expression"
                else
                    None
            | _ -> None

        Option.orElse patternCheck (Option.orElse maxCheck minCheck)

    member this.Read(value: JsonElement) : Result<Pulumi.Provider.PropertyValue, string> =
        if value.ValueKind = JsonValueKind.String then
            let str = value.GetString()

            match this.Validate str with
            | Some err -> Error err
            | None -> Ok(Pulumi.Provider.PropertyValue str)
        else
            Error(sprintf "Value is \"%s\" but should be \"string\"" (Schema.typeOfValue value))

    member this.Write(value: Pulumi.Provider.PropertyValue) : Result<JsonNode option, string> =
        match value.TryGetString() with
        | false, _ -> Error(sprintf "Value is \"%O\" but should be \"string\"" value.Type)
        | true, str ->
            match this.Validate str with
            | Some err -> Error err
            | None -> Ok(Some(JsonValue.Create str))
