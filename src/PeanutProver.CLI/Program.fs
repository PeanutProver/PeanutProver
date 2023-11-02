open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open PeanutProver.CLI
open System

[<EntryPoint>]
let main args =
    use host =
        Host
            .CreateDefaultBuilder(args)
            .ConfigureServices(fun hostContext services -> services.AddSingleton<MainAsync>() |> ignore)
            .Build()

    use scope = host.Services.CreateScope()
    let main = scope.ServiceProvider.GetRequiredService<MainAsync>()
    let lt = scope.ServiceProvider.GetRequiredService<IHostApplicationLifetime>()

    Console.CancelKeyPress
    |> Event.add (fun evtarg ->
        lt.StopApplication()
        evtarg.Cancel <- true)

    let mutable exitCode = 1

    // TODO: Probably there must be try-catch
    exitCode <- main.Run() |> Async.RunSynchronously

    exitCode
