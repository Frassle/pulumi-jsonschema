namespace JsonSchema

open System
open System.Text.Json

type Provider(conversion: Converter.RootConversion, host: Pulumi.Provider.IHost) =
    inherit Pulumi.Provider.Provider()

    override this.GetSchema
        (
            request: Pulumi.Provider.GetSchemaRequest,
            cancellationToken: System.Threading.CancellationToken
        ) =
        async {
            let resp = Pulumi.Provider.GetSchemaResponse()
            resp.Schema <- conversion.Schema.ToJsonString()
            return resp
        }
        |> fun computation ->
            Async.StartAsTask(computation, Threading.Tasks.TaskCreationOptions.None, cancellationToken)

    override this.Invoke
        (
            request: Pulumi.Provider.InvokeRequest,
            cancellationToken: System.Threading.CancellationToken
        ) =
        async {
            let resp = Pulumi.Provider.InvokeResponse()

            if request.Tok = "jsonschema:index:read" then
                match request.Args.TryGetValue "json" with
                | false, _ -> failwith "Expected an input property 'json'"
                | true, jsonProperty ->
                    match jsonProperty.TryGetString() with
                    | false, _ -> failwith "Expected input property 'json' to be a string"
                    | true, jsonText ->
                        let jsonData = System.Text.Encoding.UTF8.GetBytes jsonText
                        let mutable reader = Utf8JsonReader(jsonData)
                        let jsonElement = JsonElement.ParseValue(&reader)
                        let result = conversion.Reader jsonElement
                        resp.Return <- dict [ "value", result ]
            elif request.Tok = "jsonschema:index:write" then
                match request.Args.TryGetValue "value" with
                | false, _ -> failwith "Expected an input property 'value'"
                | true, valueProperty ->
                    let jsonNode = conversion.Writer valueProperty

                    match jsonNode with
                    | None -> resp.Return <- dict [ "value", Pulumi.Provider.PropertyValue("null") ]
                    | Some node -> resp.Return <- dict [ "value", Pulumi.Provider.PropertyValue(node.ToJsonString()) ]
            else
                failwithf "Unknown invoke '%s'" request.Tok

            return resp
        }
        |> fun computation ->
            Async.StartAsTask(computation, Threading.Tasks.TaskCreationOptions.None, cancellationToken)

module Provider =
    [<EntryPoint>]
    let main args =
        use cts = new System.Threading.CancellationTokenSource()

        let uri, schema =
            if args.Length > 2 then
                let contents = System.IO.File.ReadAllBytes(args[1])
                Uri(System.IO.Path.GetFullPath(args[1])), System.Text.Json.JsonDocument.Parse(contents)
            else
                // Default to the pulumi schema to help with testing
                use client = new System.Net.Http.HttpClient()

                let uri =
                    Uri("https://raw.githubusercontent.com/pulumi/pulumi/master/pkg/codegen/schema/pulumi.json")

                let contents = client.GetStringAsync(uri)
                uri, System.Text.Json.JsonDocument.Parse(contents.Result)

        let conversion = Converter.convertSchema uri schema.RootElement

        let task =
            Pulumi.Provider.Provider.Serve(args, (fun host -> Provider(conversion, host)), cts.Token)

        task.Wait()
        0
