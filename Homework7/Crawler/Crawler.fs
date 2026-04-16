module WebCrawler

open System
open System.Net.Http
open System.Text.RegularExpressions
open System.Collections.Concurrent
open System.Threading.Tasks

module Crawler =

    type PageResult = {
        Url: string
        Size: int
    }

    let rec private processPagePrintAsync 
        (url: string)
        (visited: ConcurrentDictionary<string, bool>)
        (httpClient: HttpClient)
        (linkRegex: Regex) : Task =

        task {
            if not (visited.TryAdd(url, true)) then return ()

            try
                let! html = httpClient.GetStringAsync(url)
                Console.WriteLine($"{url} — {html.Length} символов")

                let matches = linkRegex.Matches(html)
                let tasks = ResizeArray<Task>()

                for m in matches do
                    let link = m.Groups[1].Value.Trim()

                    let mutable resolvedUri = null : Uri
                    let absoluteUrl =
                        if Uri.TryCreate(Uri(url), link, &resolvedUri) then
                            resolvedUri.AbsoluteUri
                        else
                            Uri(Uri(url), link).AbsoluteUri

                    if absoluteUrl.StartsWith("http://") || absoluteUrl.StartsWith("https://") then
                        tasks.Add(processPagePrintAsync absoluteUrl visited httpClient linkRegex)

                if tasks.Count > 0 then
                    do! Task.WhenAll(tasks)

            with _ -> ()
        }

    let rec private processPageWithResultsAsync 
        (url: string)
        (visited: ConcurrentDictionary<string, bool>)
        (httpClient: HttpClient)
        (linkRegex: Regex)
        (results: ResizeArray<PageResult>) : Task =

        task {
            if not (visited.TryAdd(url, true)) then return ()

            try
                let! html = httpClient.GetStringAsync(url)
                results.Add { Url = url; Size = html.Length }

                let matches = linkRegex.Matches(html)
                let tasks = ResizeArray<Task>()

                for m in matches do
                    let link = m.Groups[1].Value.Trim()

                    let mutable resolvedUri = null : Uri
                    let absoluteUrl =
                        if Uri.TryCreate(Uri(url), link, &resolvedUri) then
                            resolvedUri.AbsoluteUri
                        else
                            Uri(Uri(url), link).AbsoluteUri

                    if absoluteUrl.StartsWith("http://") || absoluteUrl.StartsWith("https://") then
                        tasks.Add(processPageWithResultsAsync absoluteUrl visited httpClient linkRegex results)

                if tasks.Count > 0 then
                    do! Task.WhenAll(tasks)

            with _ -> ()
        }

    let crawlAndPrintAsync (startUrl: string) : Task =
        task {
            let visited = ConcurrentDictionary<string, bool>()
            use httpClient = new HttpClient(Timeout = TimeSpan.FromSeconds(30.0))

            let linkRegex = Regex(@"<a\s+[^>]*href\s*=\s*[""']([^""']+)[""']", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

            do! processPagePrintAsync startUrl visited httpClient linkRegex
        }

    let crawlAsync (startUrl: string) (httpClient: HttpClient) : Task<PageResult list> =
        task {
            let visited = ConcurrentDictionary<string, bool>()

            let linkRegex = Regex(@"<a\s+[^>]*href\s*=\s*[""']([^""']+)[""']", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

            let results = ResizeArray<PageResult>()

            do! processPageWithResultsAsync startUrl visited httpClient linkRegex results

            return results |> Seq.toList
        }