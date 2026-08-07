package io.fele.mahjong.server

/** Which browser origins may talk to this server (issue #52).
  *
  * Parsed from `server.allowedOrigins` / `MAHJONG_ALLOWED_ORIGINS`: a comma
  * separated list of exact origins, or `"*"` to allow any (the dev default,
  * so a checkout still works with `pnpm dev` on an arbitrary port).
  *
  * Matching is on the whole origin string — scheme, host and port — because
  * that is what the browser sends and what actually bounds access. Host-only
  * matching would let `http://` in when only `https://` was intended. */
final class OriginPolicy private (private val allowed: Option[Set[String]]) {

  /** True when no allowlist is configured, i.e. any origin is accepted. */
  val allowsAll: Boolean = allowed.isEmpty

  /** Is this `Origin` header value permitted? */
  def permits(origin: String): Boolean =
    allowed.forall(_.contains(OriginPolicy.normalize(origin)))

  /** The configured origins, for logging. */
  def describe: String = allowed.fold("* (any origin)")(_.toList.sorted.mkString(", "))
}

object OriginPolicy {

  val allowAll: OriginPolicy = new OriginPolicy(None)

  /** Trailing slashes and case are not significant in an origin; browsers send
    * neither, but hand-written config routinely has both. */
  private def normalize(o: String): String = {
    val t = o.trim.toLowerCase
    if (t.endsWith("/")) t.dropRight(1) else t
  }

  /** An empty or `*`-containing list means "no restriction": a half-configured
    * allowlist that silently blocked everyone would be worse than the default. */
  def fromConfig(raw: String): OriginPolicy = {
    val entries = raw.split(",").map(normalize).filter(_.nonEmpty)
    if (entries.isEmpty || entries.contains("*")) allowAll
    else new OriginPolicy(Some(entries.toSet))
  }
}
