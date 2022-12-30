module SdkTests

open System
open System.Text.Json
open Xunit

let writeSdk (schema: System.Text.Json.Nodes.JsonObject) (name: string) =
    // And now we can re-generate the SDK for this!
    System.IO.File.WriteAllText(name + ".json", schema.ToJsonString())

    let si = System.Diagnostics.ProcessStartInfo("pulumi")

    for arg in
        [ "package"
          "gen-sdk"
          System.IO.Path.GetFullPath(name + ".json")
          "--language"
          "dotnet"
          "--out"
          "./Examples/" + name ] do
        si.ArgumentList.Add arg

    si.RedirectStandardOutput <- true
    si.RedirectStandardError <- true
    // Find the root directory
    let mutable cwd = System.Environment.CurrentDirectory

    while System.IO.Path.GetFileName cwd <> "Tests" do
        cwd <- System.IO.Path.GetDirectoryName cwd
    // Go up one more directory
    cwd <- System.IO.Path.GetDirectoryName cwd
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

    System.IO.File.WriteAllText(System.IO.Path.Combine(cwd, "Examples", name, "dotnet", "version.txt"), "")
    let options = System.Text.Json.JsonSerializerOptions()
    options.WriteIndented <- true

    System.IO.File.WriteAllText(
        System.IO.Path.Combine(cwd, "Examples", name, "dotnet", "schema.json"),
        schema.ToJsonString(options)
    )

[<Fact>]
let ``Test githhub`` () =
    let uri =
        Uri("https://raw.githubusercontent.com/SchemaStore/schemastore/master/src/schemas/json/github-workflow.json")

    let schema =
        use client = new System.Net.Http.HttpClient()
        let contents = client.GetStringAsync(uri)
        System.Text.Json.JsonDocument.Parse contents.Result

    let conversion = JsonSchema.Converter.convertSchema uri schema.RootElement

    Assert.NotNull(conversion)
    writeSdk conversion.Schema "github"

[<Fact>]
let ``Test pulumi`` () =
    let uri =
        Uri("https://raw.githubusercontent.com/pulumi/pulumi/master/pkg/codegen/schema/pulumi.json")

    let schema =
        use client = new System.Net.Http.HttpClient()
        let contents = client.GetStringAsync(uri)
        System.Text.Json.JsonDocument.Parse contents.Result

    let conversion = JsonSchema.Converter.convertSchema uri schema.RootElement

    Assert.NotNull(conversion)
    writeSdk conversion.Schema "pulumi"

    // What's "fun" is that this is the schema for pulumi schema,
    // and so we've generated a pulumi schema for pulumi schema,
    // which means it should be able to read itself
    let pulumiSchemaDocument = conversion.Schema.Deserialize<JsonElement>()
    let dom = conversion.Reader pulumiSchemaDocument
    Assert.NotNull(dom)
