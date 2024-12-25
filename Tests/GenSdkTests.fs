module GenSdkTests

open Xunit

let genSdk (parameter: string) (name: string) =
    // Find the root directory
    let mutable cwd = System.Environment.CurrentDirectory

    while System.IO.Path.GetFileName cwd <> "Tests" do
        cwd <- System.IO.Path.GetDirectoryName cwd
    // Go up one more directory
    cwd <- System.IO.Path.GetDirectoryName cwd

    // And now we can re-generate the SDK for this!
    let providerPath = System.IO.Path.Combine(cwd, "Provider")

    let si = System.Diagnostics.ProcessStartInfo("pulumi")

    for arg in
        [ "package"
          "gen-sdk"
          providerPath
          parameter
          "--language"
          "nodejs"
          "--out"
          "./Examples/typescript" + name ] do
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

[<Fact>]
let ``Test pulumi`` () =
    genSdk "https://raw.githubusercontent.com/pulumi/pulumi/master/pkg/codegen/schema/pulumi.json" "pulumi"