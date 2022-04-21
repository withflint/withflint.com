namespace withflint.com

module Blog =
  open System
  open System.IO

  let root = Directory.GetParent(Directory.GetCurrentDirectory())

  type Meta =
    { Type: string
      Title: string
      Url: string
      Image: string
      Description: string
      Author: string
      PublishedTime: string }

  type Article =
    { Author: string
      Bio: string
      Link: string
      Avatar: string
      Slug: string
      Date: string
      Title: string
      Sub: string
      Body: string
      Meta: Meta }

  let lines (path: string) : string list =
    seq { yield! File.ReadLines path } |> Seq.toList

  let generateMeta meta =
    [ $"""<meta property="og:type" content="{meta.Type}" />"""
      $"""<meta property="og:title" content="{meta.Title}" />"""
      $"""<meta property="og:url" content="{meta.Url}" />"""
      $"""<meta property="og:image" content="{meta.Image}" />"""
      $"""<meta property="og:description" content="{meta.Description}" />"""
      $"""<meta property="article:author" content="{meta.Author}" />"""
      $"""<meta property="article:publishedTime" content="{meta.PublishedTime}" />""" ]
    |> String.concat "\n    "

  let publishedTime (date: string) =
    match System.DateTime.TryParse date with
    | true, r -> r
    | _ -> DateTime.Now
    |> (fun date -> date.ToShortDateString())

  let parseArticle =
    function
    | (author :: bio :: link :: avatar :: _ :: slug :: date :: title :: sub :: _ :: body) ->
      Some
        { Author = author
          Bio = bio
          Link = link
          Avatar = avatar
          Slug = slug
          Date = date
          Title = title
          Sub = sub
          Body = String.concat "\n" <| body
          Meta =
            { Type = "article"
              Title = title
              Url = "https://withflint.com/blog/" + slug
              Image = $"https://withflint.com/static/images/blog/{slug}.jpeg"
              Description = sub
              Author = link
              PublishedTime = publishedTime date } }
    | _ -> None

  let articles =
    Directory.GetFiles($"{root}/content/blog/", "*.md")
    |> Seq.sortDescending
    |> Seq.toList
    |> List.map (lines >> parseArticle)
    |> List.choose id


module Jobs =
  open System.IO

  type Job =
    { Url: string
      Title: string
      Location: string
      Equity: string
      Experience: string
      Description: string }

  let root = Directory.GetParent(Directory.GetCurrentDirectory())

  let lines (path: string) : string list =
    seq { yield! File.ReadLines path } |> Seq.toList

  let parseJob =
    function
    | (url :: title :: location :: equity :: experience :: _ :: body) ->
      Some(
        url,
        { Url = url
          Title = title
          Location = location
          Equity = equity
          Experience = experience
          Description = String.concat "\n" <| body }
      )
    | _ -> None

  let flint =
    Directory.GetFiles($"{root}/content/jobs/flint/", "*.md")
    |> Seq.sortDescending
    |> Seq.map (lines >> parseJob)
    |> Seq.choose id
    |> Map

  let healthcare =
    Directory.GetFiles($"{root}/content/jobs/healthcare/", "*.md")
    |> Seq.sortDescending
    |> Seq.map (lines >> parseJob)
    |> Seq.choose id
    |> Map

module Sitemap =
  open System.IO

  open Blog

  let sitemap articles =
    (File.ReadAllText "../static/sitemap.xml")
      .Replace(
        "<articles />",
        articles
        |> List.map (fun article -> $"<url><loc>https://withflint.com/blog/{article.Slug}</loc></url>")
        |> String.concat "\n  "
      )

module Apply =
  open System
  open System.IO
  open System.Net.Mail
  open System.Threading

  open Microsoft.AspNetCore.Http

  type Config =
    { who: string
      from: string
      email: string }

  type Candidate =
    { applicationTitle: string
      firstName: string
      lastName: string
      email: string
      phone: string
      reason: string }

  let candidate form =
    { applicationTitle = form ("applicationTitle")
      firstName = form "firstName"
      lastName = form "lastName"
      email = form "email"
      phone = form "phone"
      reason = form "reason" }

  let now (ctx: HttpContext) (conf: Config) candidate =
    printfn "Application received %A" candidate

    let attachments =
      (match ctx.Request.HasFormContentType with
       | false -> []
       | true ->
         ctx.Request.Form.Files
         |> Seq.fold
              (fun acc file ->

                let target = MemoryStream()

                file.CopyToAsync(target, CancellationToken(false))
                |> Async.AwaitTask
                |> Async.RunSynchronously

                target.Position <- 0L

                acc
                @ [ (target, file.ContentType, file.FileName) ])
              [])

    let body =
      $"""{candidate.reason}<br><br>{candidate.firstName} {candidate.lastName}<br>{candidate.phone}<br>{candidate.email}"""

    use smtpClient = SmtpClient("smtp-relay.gmail.com", 587)

    smtpClient.EnableSsl <- true

    use jobs = MailMessage(conf.who, conf.who)

    attachments
    |> List.iter (fun (contents, contentType, fileName) ->
      jobs.Attachments.Add(Attachment(contents, fileName, contentType)))

    jobs.Subject <-
      $"Flint - New Application : {candidate.firstName} {candidate.lastName}, {candidate.applicationTitle}"

    jobs.Body <- body
    jobs.IsBodyHtml <- true
    jobs.ReplyToList.Add(candidate.email)

    smtpClient.Send(jobs)

    attachments
    |> List.iter (fun (stream, _, _) -> stream.Dispose())

    use candidate = MailMessage(conf.from, candidate.email)

    candidate.Subject <- "Thank you for applying"
    candidate.IsBodyHtml <- true

    candidate.Body <- conf.email

    smtpClient.Send(candidate)

module Program =
  open System
  open System.IO

  open Microsoft.AspNetCore.Builder
  open Microsoft.AspNetCore.Hosting
  open Microsoft.Extensions.Hosting
  open Microsoft.Extensions.DependencyInjection
  open Microsoft.AspNetCore.Http
  open Microsoft.Extensions.FileProviders
  open Microsoft.AspNetCore.Rewrite

  open Giraffe
  open FSharp.Control.Tasks.Affine

  let print a =
    printfn "%A" a
    a

  let root = Directory.GetParent(Directory.GetCurrentDirectory())

  let privacy =
    File.ReadAllText "../static/privacy.html"
    |> htmlString

  let indexWithMeta = File.ReadAllText "../index.html"

  let index = indexWithMeta.Replace("<meta />", "")

  let htmlIndex = htmlString index

  let healthz =
    let env = Environment.GetEnvironmentVariable("ENV")

    let gv = Environment.GetEnvironmentVariable("GIT_VERSION")

    let now = DateTime.Now.ToUniversalTime()

    text $"Ok,{env},{gv},{now}"

  let blogs (next: HttpFunc) (ctx: HttpContext) =
    task { return! (Blog.articles |> json) next ctx }

  let yc (next: HttpFunc) (ctx: HttpContext) =
    task { return! (Jobs.flint |> json) next ctx }

  let hc (next: HttpFunc) (ctx: HttpContext) =
    task { return! (Jobs.healthcare |> json) next ctx }

  let apply =
    fun (next: HttpFunc) (ctx: HttpContext) ->
      task {
        let form key = ctx.Request.Form.Item(key).ToString()

        let candidate = Apply.candidate form

        let email =
          $"""Hello {candidate.firstName},<br><br>Thank you for your interest in the {candidate.applicationTitle} position at Flint.<br>I will review your candidacy and get back to you shortly.<br><br>Kind Regards,<br><br>Simon Green<br>Head of Engineering at <a href='https://withflint.com/'> Flint </a>"""

        Apply.now
          ctx
          { who = "careers+ws@withflint.com"
            from = "Simon Green<simon@withflint.com>"
            email = email }
          candidate

        return! json {| success = "OK" |} next ctx
      }

  let happly =
    fun (next: HttpFunc) (ctx: HttpContext) ->
      task {
        let form key = ctx.Request.Form.Item(key).ToString()

        let candidate = Apply.candidate form

        let email =
          $"""Hello {candidate.firstName},<br><br>Thank you for your interest in the {candidate.applicationTitle} position with Flint.<br>We will review your candidacy and get back to you shortly.<br><br>Kind Regards,<br><br>The Talent Team at <a href='https://withflint.com/'> Flint </a>"""

        Apply.now
          ctx
          { who = "Flint Talent Team<talent@withflint.com>"
            from = "Flint Talent Team<talent@withflint.com>"
            email = email }
          candidate

        return! json {| success = "OK" |} next ctx
      }

  let articleWithMeta slug =
    fun (next: HttpFunc) (ctx: HttpContext) ->
      task {
        let meta =
          Blog.articles
          |> List.filter (fun article -> article.Slug = slug)
          |> List.map (fun article -> article.Meta)
          |> List.head
          |> Blog.generateMeta

        return! htmlString (indexWithMeta.Replace("<meta />", meta)) next ctx

      }

  let webApp: HttpHandler =
    choose [ GET
             >=> choose [ route "/" >=> htmlIndex
                          route "/hc" >=> hc
                          route "/yc" >=> yc
                          route "/privacy" >=> privacy
                          route "/faq" >=> redirectTo false "/"
                          route "/faq/" >=> redirectTo false "/"
                          route "/careers" >=> redirectTo true "/jobs"
                          route "/careers/" >=> redirectTo true "/jobs"
                          route "/team" >=> redirectTo true "/"
                          route "/team/" >=> redirectTo true "/"
                          subRoute
                            "/blog"
                            (choose [ route "" >=> htmlIndex
                                      routef "/%s" articleWithMeta ])
                          route "/articles" >=> blogs
                          route "/healthz" >=> healthz
                          route "/sitemap.xml"
                          >=> (Blog.articles |> Sitemap.sitemap |> htmlString) ]
             POST >=> choose [ route "/apply" >=> apply ]
             POST >=> choose [ route "/happly" >=> happly ]
             setStatusCode 200 >=> htmlIndex ]

  let configureApp (app: IApplicationBuilder) =
    let path = Path.Combine(Directory.GetCurrentDirectory(), @"../static")

    use provider = PhysicalFileProvider(path)

    app
      .UseRewriter(
        (RewriteOptions())
          .AddRewrite("favicon.ico", "static/favicon.ico", false)
      )
      .UseDefaultFiles()
      .UseStaticFiles(StaticFileOptions(FileProvider = provider, RequestPath = PathString("/static")))
      .UseGiraffe(webApp)

  let configureServices (services: IServiceCollection) = services.AddGiraffe() |> ignore

  [<EntryPoint>]
  let main _ =

    Host
      .CreateDefaultBuilder()
      .ConfigureWebHostDefaults(fun webHostBuilder ->
        webHostBuilder
          .UseUrls("http://+:5000")
          .Configure(configureApp)
          .ConfigureServices(configureServices)
        |> ignore)
      .Build()
      .Run()

    printfn "Exiting"

    0
