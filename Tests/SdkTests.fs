module SdkTests

open System
open System.Text.Json
open Xunit

let writeSdk (schema: System.Text.Json.Nodes.JsonObject) (name: string) =
    // Find the root directory
    let mutable cwd = System.Environment.CurrentDirectory

    while System.IO.Path.GetFileName cwd <> "Tests" do
        cwd <- System.IO.Path.GetDirectoryName cwd
    // Go up one more directory
    cwd <- System.IO.Path.GetDirectoryName cwd

    // And now we can re-generate the SDK for this!
    let options = System.Text.Json.JsonSerializerOptions()
    options.WriteIndented <- true
    let schemaPath = System.IO.Path.Combine(cwd, "Examples", "dotnet", name, "pulumi.json")
    System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName(schemaPath)) |> ignore

    System.IO.File.WriteAllText(schemaPath, schema.ToJsonString(options))

    let si = System.Diagnostics.ProcessStartInfo("pulumi")

    for arg in
        [ "package"
          "gen-sdk"
          schemaPath
          "--language"
          "dotnet"
          "--out"
          "./Examples/dotnet/" + name ] do
        si.ArgumentList.Add arg

    si.RedirectStandardOutput <- true
    si.RedirectStandardError <- true
    si.WorkingDirectory <- cwd
    let proc = new System.Diagnostics.Process()
    proc.StartInfo <- si
    let out = System.Text.StringBuilder()
    let err = System.Text.StringBuilder()
    proc.OutputDataReceived.Add(fun diag -> out.AppendLine(diag.Data) |> ignore)
    proc.ErrorDataReceived.Add(fun diag -> err.AppendLine(diag.Data) |> ignore)

    if not (proc.Start()) then
        failwith "gen-sdk failed to start"

    proc.BeginOutputReadLine()
    proc.BeginErrorReadLine()

    proc.WaitForExit()

    if proc.ExitCode <> 0 then
        failwithf "gen-sdk failed\nstdout:\n%s\nstderr:\n%s" (out.ToString()) (err.ToString())

let doTest uri name = 
    let uri =
        Uri(uri)

    let schema =
        use client = new System.Net.Http.HttpClient()
        let contents = client.GetStringAsync(uri)
        System.Text.Json.JsonDocument.Parse contents.Result

    let conversion = JsonSchema.Converter.convertSchema uri name schema.RootElement

    Assert.NotNull(conversion)
    writeSdk conversion.Schema name
    conversion


[<Fact(Skip="allOf title not currently working")>]
let ``Test githhub`` () =
    doTest "https://raw.githubusercontent.com/SchemaStore/schemastore/master/src/schemas/json/github-workflow.json" "github"

[<Fact(Skip="Needs more translation map [(Json.Schema.RefKeyword, [Json.Schema.RefKeyword; Json.Schema.RefKeyword; Json.Schema.RefKeyword; ... ]); (Json.Schema.UnrecognizedKeyword, [Json.Schema.UnrecognizedKeyword; Json.Schema.UnrecognizedKeyword])]")>]
let ``Test tsconfig`` () =
    doTest "https://raw.githubusercontent.com/SchemaStore/schemastore/master/src/schemas/json/tsconfig.json" "tsconfig"

[<Fact>]
let ``Test cargo`` () =
    doTest "https://raw.githubusercontent.com/SchemaStore/schemastore/master/src/schemas/json/cargo.json" "cargo"

[<Fact>]
let ``Test bootstraprc`` () =
    doTest "https://raw.githubusercontent.com/SchemaStore/schemastore/master/src/schemas/json/bootstraprc.json" "bootstraprc"

[<Fact>]
let ``Test pulumi`` () =
    let conversion = doTest "https://raw.githubusercontent.com/pulumi/pulumi/master/pkg/codegen/schema/pulumi.json" "pulumi"

    // What's "fun" is that this is the schema for pulumi schema,
    // and so we've generated a pulumi schema for pulumi schema,
    // which means it should be able to read itself
    let pulumiSchemaDocument = conversion.Schema.Deserialize<JsonElement>()
    let dom = conversion.Reader pulumiSchemaDocument
    Assert.NotNull(dom)
