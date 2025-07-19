#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# generate_report.R  – Scrape, summarise, render PDF, upload to Supabase,
#                      and email the report through Mailjet
# ---------------------------------------------------------------------------

# 0 ── PACKAGES ──────────────────────────────────────────────────────────────
required <- c(
  "tidyverse", "lubridate", "httr2", "httr", "jsonlite", "glue", "pagedown",
  "RPostgres", "DBI", "base64enc", "tidytext"
)
invisible(lapply(required, \(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

# 1 ── ENVIRONMENT VARIABLES --------------------------------------------------
trim_env <- \(var) stringr::str_trim(Sys.getenv(var))

## ---- Supabase (database + storage) ----
SB_HOST        <- trim_env("SUPABASE_HOST")
SB_PORT        <- as.integer(trim_env("SUPABASE_PORT", "6543"))
SB_DB          <- trim_env("SUPABASE_DB")
SB_USER        <- trim_env("SUPABASE_USER")
SB_PWD         <- trim_env("SUPABASE_PWD")
SB_URL         <- trim_env("SUPABASE_URL")            # https://<ref>.supabase.co
SB_STORAGE_KEY <- trim_env("SUPABASE_SERVICE_ROLE")   # service‑role key
SB_BUCKET      <- trim_env("SB_BUCKET") %||% "weekly-reports"

## ---- OpenAI ----
OPENAI_KEY     <- trim_env("OPENAI_API_KEY")

## ---- Mailjet ----
MJ_API_KEY     <- trim_env("MJ_API_KEY")
MJ_API_SECRET  <- trim_env("MJ_API_SECRET")
MAIL_FROM      <- trim_env("MAIL_FROM")   # e.g. "José Peña <jgpena@uc.cl>"
MAIL_TO        <- trim_env("MAIL_TO")     # e.g. "ecotools@arweave.org.com"

stopifnot(
  nchar(SB_HOST)        > 0,
  nchar(SB_USER)        > 0,
  nchar(SB_PWD)         > 0,
  nchar(SB_DB)          > 0,
  nchar(OPENAI_KEY)     > 0,
  nchar(MJ_API_KEY)     > 0,
  nchar(MJ_API_SECRET)  > 0,
  nchar(MAIL_FROM)      > 0,
  nchar(MAIL_TO)        > 0
)

# 2 ── LOAD DATA FROM SUPABASE -----------------------------------------------
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = SB_HOST,
  port     = SB_PORT,
  dbname   = SB_DB,
  user     = SB_USER,
  password = SB_PWD,
  sslmode  = "require"
)

twitter_raw <- DBI::dbReadTable(con, "twitter_raw") |> as_tibble()

main_ids <- tribble(
  ~username,        ~main_id,
  "samecwilliams",  "409642632",
  "ar_io_network",  "1468980765211955205",
  "aoTheComputer",  "1750584639385939968"
)

tweets_tagged <- twitter_raw |>
  left_join(main_ids, by = "username") |>
  mutate(
    is_rt_text = str_detect(text, "^RT @"),
    tweet_type = case_when(
      is_rt_text &
        user_id != main_id                 ~ "retweet",
      user_id == main_id &
        is_rt_text                         ~ "quote",
      user_id == main_id                  ~ "original",
      TRUE                                 ~ "other"
    )
  )

df <- tweets_tagged |>
  mutate(
    publish_dt = ymd_hms(date),
    text       = str_squish(text),
    id         = tweet_id
  ) |>
  filter(publish_dt >= Sys.time() - ddays(7)) |>
  distinct(id, .keep_all = TRUE)

# 3 ── GPT HELPER -------------------------------------------------------------
ask_gpt <- function(prompt, model = "gpt-4o-mini", temperature = 0,
                    max_tokens = 700, retries = 3) {
  for (k in seq_len(retries)) {
    resp <- tryCatch({
      request("https://api.openai.com/v1/chat/completions") |>
        req_method("POST") |>
        req_headers(Authorization = paste("Bearer", OPENAI_KEY)) |>
        req_body_json(list(
          model       = model,
          temperature = temperature,
          max_tokens  = max_tokens,
          messages    = list(
            list(role = "system", content = "You are a concise analyst."),
            list(role = "user",   content = prompt)
          )
        )) |>
        req_retry(max_tries = 3) |>
        req_perform()
    }, error = identity)
    if (!inherits(resp, "error") && resp_status(resp) == 200) {
      return(resp_body_json(resp)$choices[[1]]$message$content |> str_trim())
    }
    Sys.sleep(2 ^ k)
  }
  stop("All OpenAI retries failed")
}

# 4 ── BUILD PROMPTS (shortened) ---------------------------------------------
first60 <- \(txt) str_sub(txt, 1, 60)

tweet_lines <- df |>
  mutate(
    line = glue("{format(publish_dt, '%Y-%m-%d %H:%M')} | ",
                "ER={round(engagement_rate, 4)}% | ",
                "{first60(text)} | {tweet_url}")
  ) |>
  pull(line)
big_text <- paste(tweet_lines, collapse = "\n")

overall_prompt <- glue(
  "Below is a collection of tweets; each line is:",
  " URL | Date | Engagement Rate | Tweet text.\n\n",
  "Write ONE concise bullet‑point summary of concrete activities, events, ",
  "and product launches from the last 7 days.\n",
  "• **Headline** (≤20 words) + `YYYY-MM-DD`.\n",
  "• Two‑space indent → first 60 chars of tweet + URL.\n\n",
  big_text
)
overall_summary <- ask_gpt(overall_prompt)

# 5 ── RENDER MARKDOWN → PDF --------------------------------------------------
writeLines(c("# Weekly Summary", "", overall_summary), "summary.md")
pagedown::chrome_print("summary.md", output = "summary_full.pdf")

# 6 ── UPLOAD TO SUPABASE STORAGE --------------------------------------------
object_path <- sprintf(
  "%s/summary_%s.pdf",
  format(Sys.Date(), "%Yw%V"),
  format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
)
upload_url <- sprintf(
  "%s/storage/v1/object/%s/%s?upload=1",
  SB_URL, SB_BUCKET, URLencode(object_path, TRUE)
)

request(upload_url) |>
  req_method("POST") |>
  req_headers(
    Authorization = paste("Bearer", SB_STORAGE_KEY),
    "x-upsert"    = "true",
    "Content-Type"= "application/pdf"
  ) |>
  req_body_file("summary_full.pdf") |>
  req_perform()

cat("✔ Uploaded to Supabase: ", object_path, "\n")

# 7 ── EMAIL VIA MAILJET REST API --------------------------------------------
req <- request("https://api.mailjet.com/v3.1/send") |>
  req_auth_basic(MJ_API_KEY, MJ_API_SECRET) |>
  req_body_json(list(
    Messages = list(list(
      From     = list(
        Email = sub(".*<(.*)>.*", "\\1", MAIL_FROM),
        Name  = str_trim(sub("<.*>", "", MAIL_FROM))
      ),
      To       = list(list(Email = MAIL_TO)),
      Subject  = "Weekly Twitter Report",
      TextPart = "Attached you'll find the weekly report in PDF.",
      Attachments = list(list(
        ContentType   = "application/pdf",
        Filename      = "weekly_report.pdf",
        Base64Content = base64enc::base64encode("summary_full.pdf")
      ))
    ))
  )) |>
  req_perform()

stopifnot(resp_status(req) == 200)
cat("📧  Mailjet response OK – report emailed\n")

