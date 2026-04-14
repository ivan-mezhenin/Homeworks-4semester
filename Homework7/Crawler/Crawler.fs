using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Net.Http;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

var startUrl = args.Length > 0 ? args[0] : "https://example.com";

await CrawlAndPrintAsync(startUrl);

static async Task CrawlAndPrintAsync(string startUrl)
{
    var visited = new ConcurrentDictionary<string, bool>();
    var httpClient = new HttpClient { Timeout = TimeSpan.FromSeconds(30) };

    var linkRegex = new Regex(
        @"<a\s+[^>]*?href\s*=\s*[""']([^""']+)[""']",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    await ProcessPageAsync(startUrl, visited, httpClient, linkRegex);
}

static async Task ProcessPageAsync(string url, ConcurrentDictionary<string, bool> visited, HttpClient httpClient, Regex linkRegex)
{
    if (!visited.TryAdd(url, true))
        return;

    try
    {
        string html = await httpClient.GetStringAsync(url);
        Console.WriteLine($"{url} — {html.Length} символов");

        var matches = linkRegex.Matches(html);

        var tasks = new List<Task>();

        foreach (Match match in matches)
        {
            string link = match.Groups[1].Value.Trim();

            if (!Uri.TryCreate(link, UriKind.Absolute, out Uri? absoluteUri))
            {
                if (Uri.TryCreate(new Uri(url), link, out absoluteUri))
                    link = absoluteUri.AbsoluteUri;
                else
                    continue;
            }

            if (absoluteUri.Scheme is not ("http" or "https"))
                continue;

            tasks.Add(ProcessPageAsync(link, visited, httpClient, linkRegex));
        }

        if (tasks.Count > 0)
            await Task.WhenAll(tasks);
    }
    catch (Exception ex)
    {
        Console.WriteLine($"{url} — ошибка: {ex.Message}");
    }
}