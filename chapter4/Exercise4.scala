package chapter4

import java.util.regex.{Pattern, PatternSyntaxException}

object Exercise4 {
    def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] = {
        Exercise3.map2(pattern(pat1), pattern(pat2))((a, b) => a.matcher(s).matches() && b.matcher(s).matches())
    }

    def pattern(s: String): Option[Pattern] =
        try {
            Some(Pattern.compile(s))
        } catch {
            case e: PatternSyntaxException => None
        }
}
