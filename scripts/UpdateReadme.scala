//> using scala 3.5.2
//> using jvm 21
//> using dep com.lihaoyi::upickle:4.0.2

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path}

/** Refreshes the <!-- START:x --> ... <!-- END:x --> sections of README.md
  * with live data from the public GitHub REST API. */
object UpdateReadme:
  val user     = sys.env.getOrElse("GH_USER", "tomiwu")
  lazy val conf: Map[String, String] =
    val f = Path.of(sys.env.getOrElse("PROFILE_CONF", "profile.conf"))
    if !Files.exists(f) then Map.empty
    else Files.readAllLines(f).toArray(Array.empty[String]).toSeq.map(_.trim)
      .filter(l => l.nonEmpty && !l.startsWith("#") && l.contains("="))
      .map(l => l.split("=", 2) match { case Array(k, v) => k.trim -> v.trim }).toMap
  def list(key: String): Seq[String] = conf.get(key).toSeq.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty)
  lazy val excluded = list("exclude").toSet
  lazy val pinned   = list("pinned")
  lazy val maxFeat  = conf.get("featured.max").flatMap(_.toIntOption).getOrElse(6)
  lazy val months   = conf.get("featured.months").flatMap(_.toIntOption).getOrElse(12)
  val client   = HttpClient.newHttpClient()

  def get(path: String): ujson.Value =
    val b = HttpRequest.newBuilder(URI.create(s"https://api.github.com$path"))
      .header("Accept", "application/vnd.github+json")
      .header("User-Agent", s"$user-readme-updater")
    sys.env.get("GITHUB_TOKEN").filter(_.nonEmpty).foreach(t => b.header("Authorization", s"Bearer $t"))
    val r = client.send(b.build(), HttpResponse.BodyHandlers.ofString())
    require(r.statusCode == 200, s"GET $path -> ${r.statusCode}")
    ujson.read(r.body)

  def day(iso: String): String = iso.take(10)

  def prStatus(pr: ujson.Value): String =
    if pr("state").str == "open" then "🟢 open"
    else if !pr("pull_request")("merged_at").isNull then "🟣 merged"
    else "⚪ closed"

  def recentPrs(n: Int): String =
    val items = get(s"/search/issues?q=author:$user+type:pr+is:public&sort=created&order=desc&per_page=$n")("items").arr
    val rows = items.map { pr =>
      val repo = pr("repository_url").str.stripPrefix("https://api.github.com/repos/")
      s"| ${prStatus(pr)} | [${pr("title").str.replace("|", "\\|")}](${pr("html_url").str}) | `$repo` | ${day(pr("created_at").str)} |"
    }
    ("| Status | Pull request | Repository | Opened |" +: "|---|---|---|---|" +: rows).mkString("\n")

  lazy val ownRepos: Seq[ujson.Value] =
    get(s"/users/$user/repos?per_page=100&sort=pushed&type=owner").arr.toSeq
      .filterNot(r => r("fork").bool || r("archived").bool || excluded(r("name").str))

  def activeRepos(n: Int, since: String): String =
    ownRepos.filter(_("pushed_at").str >= since).take(n).map { r =>
      val lang = r("language").strOpt.fold("")(l => s" · `$l`")
      val desc = r("description").strOpt.fold("")(d => s" — $d")
      s"- [**${r("name").str}**](${r("html_url").str})$lang$desc _(last push ${day(r("pushed_at").str)})_"
    }.mkString("\n")

  def releases(n: Int): String =
    val rs = ownRepos.flatMap { r =>
      get(s"/repos/$user/${r("name").str}/releases?per_page=3").arr.filterNot(_("draft").bool).map(r("name").str -> _)
    }.sortBy(_._2("published_at").str).reverse.take(n)
    if rs.isEmpty then "_No releases yet._"
    else rs.map((repo, rel) =>
      s"- **$repo** [${rel("tag_name").str}](${rel("html_url").str}) — ${day(rel("published_at").str)}").mkString("\n")

  def featuredRepos(since: String): Seq[ujson.Value] =
    val pins   = pinned.flatMap(n => ownRepos.find(_("name").str == n))
    val recent = ownRepos.filter(_("pushed_at").str >= since).filterNot(r => pinned.contains(r("name").str))
    (pins ++ recent).take(maxFeat)

  /** First sentence of the repo README, skipping headings, badges and HTML. */
  def readmeSentence(repo: String): Option[String] =
    scala.util.Try {
      val b = get(s"/repos/$user/$repo/readme")("content").str.replace("\n", "")
      new String(java.util.Base64.getDecoder.decode(b), "UTF-8").linesIterator.map(_.trim)
        .find(l => l.nonEmpty && !l.startsWith("#") && !l.startsWith("<") && !l.startsWith("[!") && !l.startsWith("!["))
        .map(l => l.split("(?<=\\.)\\s").head)
    }.toOption.flatten

  def featured(since: String): String =
    val rows = featuredRepos(since).map { r =>
      val name   = r("name").str
      val card   = s"https://github-readme-stats.vercel.app/api/pin/?username=$user&repo=$name&theme=github_dark&hide_border=true"
      val desc   = conf.get(s"description.$name").orElse(r("description").strOpt).orElse(readmeSentence(name))
        .getOrElse("_No description yet._").replace("|", "\\|")
      val lang   = conf.get(s"language.$name").orElse(r("language").strOpt).map(l => s"`$l`")
      val topics = r.obj.get("topics").map(_.arr.map(t => s"`${t.str}`").mkString(" ")).filter(_.nonEmpty)
      val meta   = (lang.toSeq ++ Seq(s"⭐ ${r("stargazers_count").num.toInt}") ++ topics).mkString(" · ")
      val overridden = conf.contains(s"description.$name") || conf.contains(s"language.$name") || r("description").isNull
      val cell = if overridden then s"<a href=\"${r("html_url").str}\"><b>📦 $name</b></a>"
                 else s"<a href=\"${r("html_url").str}\"><img width=\"300\" src=\"$card\" alt=\"$name\" /></a>"
      val pin    = if pinned.contains(name) then "📌 " else ""
      s"| $cell | $pin**[$name](${r("html_url").str})**<br/>$desc<br/>$meta<br/><sub>last push ${day(r("pushed_at").str)}</sub> |"
    }
    if rows.isEmpty then "_Nothing active right now._"
    else ("| | |" +: "|---|---|" +: rows).mkString("\n")

  def languages(since: String): String =
    val bytes = featuredRepos(since).flatMap(r => get(s"/repos/$user/${r("name").str}/languages").obj.map((k, v) => k -> v.num.toLong))
      .groupMapReduce(_._1)(_._2)(_ + _).toSeq.sortBy(-_._2)
    val total = bytes.map(_._2).sum.toDouble
    if total == 0 then "_No data._"
    else bytes.filter(_._2 / total >= 0.01).map { (l, b) =>
      val pct = f"${b * 100 / total}%.1f"
      val enc = java.net.URLEncoder.encode(l, "UTF-8").replace("+", "%20").replace("-", "--")
      s"<img src=\"https://img.shields.io/badge/$enc-$pct%25-30363d?style=flat-square\" />"
    }.mkString("<p>\n", "\n", "\n</p>")

  def replace(doc: String, key: String, body: String): String =
    val re = s"(?s)(<!-- START:$key -->).*?(<!-- END:$key -->)".r
    re.replaceAllIn(doc, m => java.util.regex.Matcher.quoteReplacement(s"${m.group(1)}\n$body\n${m.group(2)}"))

  def main(args: Array[String]): Unit =
    val path   = Path.of(args.headOption.getOrElse("README.md"))
    val before = Files.readString(path)
    val since  = java.time.LocalDate.now().minusMonths(months).toString
    val after = Seq(
      "featured"     -> featured(since),
      "languages"    -> languages(since),
      "recent-prs"   -> recentPrs(5),
      "active-repos" -> activeRepos(5, since),
      "releases"     -> releases(5)
    ).foldLeft(before) { case (d, (k, v)) => replace(d, k, v) }
    if after != before then { Files.writeString(path, after); println("README.md updated") }
    else println("README.md unchanged")
