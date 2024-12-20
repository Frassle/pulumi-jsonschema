namespace JsonSchema

open System
open System.Text.Json
open Pulumi.Experimental.Provider

type Provider(host: IHost) =
    inherit Pulumi.Experimental.Provider.Provider()

    let mutable conversion: Converter.RootConversion option = None

    let getRootConversion() = 
        match conversion with 
        | Some rc -> rc
        | None -> failwith "Expected Parameterize to be called first"

    override this.GetSchema
        (
            request: GetSchemaRequest,
            cancellationToken: System.Threading.CancellationToken
        ) =
        async {
            let resp = GetSchemaResponse()
            let rc = getRootConversion()
            resp.Schema <- rc.Schema.ToJsonString()
            return resp
        }
        |> fun computation ->
            Async.StartAsTask(computation, Threading.Tasks.TaskCreationOptions.None, cancellationToken)

    override this.Parameterize (request: ParameterizeRequest, cancellationToken: Threading.CancellationToken): Threading.Tasks.Task<ParameterizeResponse> = 
        async {
            let rootConversion =
                match request.Parameters with 
                | :? ParametersArgs as args -> 
                    let uri = Uri(args.Args[0])
                    use client = new System.Net.Http.HttpClient()
                    let contents = client.GetStringAsync(uri)
                    let schema = System.Text.Json.JsonDocument.Parse(contents.Result)
                    let packageName = 
                        uri.Segments |> Seq.last |> System.IO.Path.GetFileNameWithoutExtension
                    Converter.convertSchema uri packageName schema.RootElement
                | :? ParametersValue as value -> 
                    let parameterValue = new System.IO.MemoryStream(value.Value.AsMemory().ToArray())
                    let parameterReader = new System.IO.BinaryReader(parameterValue)
                    let uri = Uri(parameterReader.ReadString())
                    let schema = parameterReader.ReadString()
                    let jsonSchema = System.Text.Json.JsonDocument.Parse(schema)
                    Converter.convertSchema uri value.Name jsonSchema.RootElement
                | _ -> failwith "Unrecognized parameter request"
            conversion <- Some rootConversion
            
            let name = rootConversion.Schema["name"].GetValue<string>()
            let version = rootConversion.Schema["version"].GetValue<string>()

            let resp = ParameterizeResponse(name, version)
            return resp
        }        
        |> fun computation ->
            Async.StartAsTask(computation, Threading.Tasks.TaskCreationOptions.None, cancellationToken)


    override this.Invoke
        (
            request: InvokeRequest,
            cancellationToken: System.Threading.CancellationToken
        ) =
        async {
            let resp = InvokeResponse()
            let rc = getRootConversion()

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
                        let result = rc.Reader jsonElement
                        resp.Return <- dict [ "value", result ]
            elif request.Tok = "jsonschema:index:write" then
                match request.Args.TryGetValue "value" with
                | false, _ -> failwith "Expected an input property 'value'"
                | true, valueProperty ->
                    let jsonNode = rc.Writer valueProperty

                    match jsonNode with
                    | None -> resp.Return <- dict [ "value", PropertyValue("null") ]
                    | Some node -> resp.Return <- dict [ "value", PropertyValue(node.ToJsonString()) ]
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
        let version = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version

        let task =
            Pulumi.Experimental.Provider.Provider.Serve(args, version.ToString(), (fun host -> Provider(host)), cts.Token)

        task.Wait()
        0
