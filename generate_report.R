#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# generate_report.R – Scrape, summarise, render PDF, upload to Supabase,
#                     and email the report through Mailjet
# ---------------------------------------------------------------------------

# 0 ── PACKAGES ──────────────────────────────────────────────────────────────
required <- c(
  "tidyverse", "lubridate", "httr2", "httr", "jsonlite", "glue", "pagedown",
  "RPostgres", "DBI", "base64enc", "tidytext","magrittr"
)
invisible(lapply(required, \(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, quiet = TRUE)
  library(pkg, character.only = TRUE)
}))

# 1 ── HELPERS ───────────────────────────────────────────────────────────────
trim_env <- \(var, default = "") {
  val <- stringr::str_trim(Sys.getenv(var, unset = default))
  if (identical(val, "")) default else val
}

ask_gpt <- function(prompt, model = "gpt-4o-mini",
                    temperature = 0, max_tokens = 700, retries = 3) {

  for (k in seq_len(retries)) {
    resp <- tryCatch(
      request("https://api.openai.com/v1/chat/completions") |>
        req_method("POST") |>
        req_headers(Authorization = paste("Bearer", OPENAI_KEY)) |>
        req_body_json(list(
          model       = model,
          temperature = temperature,
          max_tokens  = max_tokens,
          messages    = list(
            list(
              role    = "system",
              content = "You are a concise analyst. Summarise only concrete activities, events or product launches, in bullet points."
            ),
            list(role = "user", content = prompt)
          )
        )) |>
        req_retry(max_tries = 3) |>
        req_perform(),
      error = identity
    )

    if (!inherits(resp, "error") && resp_status(resp) == 200) {
      return(
        resp_body_json(resp)$choices[[1]]$message$content |>
          stringr::str_trim()
      )
    }
    Sys.sleep(2 ^ k)
  }
  stop("All OpenAI retries failed")
}

`%||%` <- function(a, b) if (nzchar(a)) a else b     # tiny helper

# 2 ── ENVIRONMENT VARIABLES -------------------------------------------------
SB_HOST        <- trim_env("SUPABASE_HOST")
SB_PORT        <- as.integer(trim_env("SUPABASE_PORT", "6543"))
SB_DB          <- trim_env("SUPABASE_DB")
SB_USER        <- trim_env("SUPABASE_USER")
SB_PWD         <- trim_env("SUPABASE_PWD")
SB_URL         <- trim_env("SUPABASE_URL")          # https://<ref>.supabase.co
SB_STORAGE_KEY <- trim_env("SUPABASE_SERVICE_ROLE")
SB_BUCKET      <- trim_env("SB_BUCKET", "weekly-reports")

OPENAI_KEY     <- trim_env("OPENAI_API_KEY")

MJ_API_KEY     <- trim_env("MJ_API_KEY")
MJ_API_SECRET  <- trim_env("MJ_API_SECRET")
MAIL_FROM      <- trim_env("MAIL_FROM")             # "José Peña <jgpena@uc.cl>" or bare address
MAIL_TO        <- trim_env("MAIL_TO")               # "ecotools@arweave.org.com"

stopifnot(
  SB_HOST  != "", OPENAI_KEY != "",
  MJ_API_KEY != "", MJ_API_SECRET != "",
  MAIL_FROM != "", MAIL_TO != ""
)

# 3 ── LOAD DATA FROM SUPABASE ----------------------------------------------
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

# ── UPDATED: full list of account → canonical‑ID mappings ───────────────────
main_ids <- tibble::tribble(
  ~username,            ~main_id,
  "weave_db",           "1206153294680403968",
  "OdyseeTeam",         "1280241715987660801",
  "ardriveapp",         "1293193263579635712",
  "redstone_defi",      "1294053547630362630",
  "everpay_io",         "1334504432973848577",
  "decentlandlabs",     "1352388512788656136",
  "KYVENetwork",        "136377177683878784",
  "onlyarweave",        "1393171138436534272",
  "ar_io_network",      "1468980765211955205",
  "Permaswap",          "1496714415231717380",
  "communitylabs",      "1548502833401516032",
  "usewander",          "1559946771115163651",
  "apus_network",       "1569621659468054528",
  "fwdresearch",        "1573616135651545088",
  "perma_dao",          "1595075970309857280",
  "Copus_io",           "1610731228130312194",
  "basejumpxyz",        "1612781645588742145",
  "AnyoneFDN",          "1626376419268784130",
  "arweaveindia",       "1670147900033343489",
  "useload",            "1734941279379759105",
  "protocolland",       "1737805485326401536",
  "aoTheComputer",      "1750584639385939968",
  "ArweaveOasis",       "1750723327315030016",
  "aox_xyz",            "1751903735318720512",
  "astrousd",           "1761104764899606528",
  "PerplexFi",          "1775862139980226560",
  "autonomous_af",      "1777500373378322432",
  "Liquid_Ops",         "1795772412396507136",
  "ar_aostore",         "1797632049202794496",
  "FusionFiPro",        "1865790600462921728",
  "vela_ventures",      "1869466343000444928",
  "beaconwallet",       "1879152602681585664",
  "VentoSwap",          "1889714966321893376",
  "permawebjournal",    "1901592191065300993",
  "Botega_AF",          "1902521779161292800",
  "samecwilliams",      "409642632",
  "TateBerenbaum",      "801518825690824707",
  "ArweaveEco",         "892752981736779776"
)


# 4 ── PRE‑PROCESS TWEETS ----------------------------------------------------
tweets <- twitter_raw |>
  left_join(main_ids, by = "username") |>
  mutate(
    is_rt_text = str_detect(text, "^RT @"),
    tweet_type = case_when(
      is_rt_text                                   ~ "retweet",
      user_id == main_id & !is_rt_text & str_detect(text, "https://t.co") ~ "quote",
      user_id == main_id                           ~ "original",
      TRUE                                         ~ "other"
    ),
    publish_dt = lubridate::ymd_hms(date, tz = "UTC"),
    text       = str_squish(text)
  ) |>
  filter(publish_dt >= Sys.time() - lubridate::ddays(7)) |>
  distinct(tweet_id, .keep_all = TRUE)

df  <- tweets |> filter(tweet_type == "original")
df2 <- tweets                                 # full set

# 5 ── SECTION 1 – LAUNCH / ACTIVITY SUMMARY ---------------------------------
# ───────── 5.1  compact tweet lines ────────────────────────────────────────
tweet_lines <- df |>
  mutate(
    line = glue(
      "{format(publish_dt, '%Y-%m-%d %H:%M')} | ",
      "@{username} | ",
      "ER={round(engagement_rate, 4)}% | ",
      "{str_replace_all(str_trunc(text, 200), '\\n', ' ')} | ",
      "{tweet_url}"
    )
  ) |>
  pull(line)


big_text <- paste(tweet_lines, collapse = "\n")

# ───────── 5.2  GPT prompt  ────────────────────────────────────────────────
prompt1 <- glue(
  "Below is a collection of tweets; each line is\n",
  "Date | Account | Engagement Rate | Tweet text | URL.\n\n",   # ← fixed order
  "Write ONE concise bullet-point summary of all concrete activities, events, ",
  "and product launches mentioned across the entire set.\n",
  "• **Begin** with date and account, e.g. `2025-08-06 (@redstone_defi): …`.\n",
  "• ≤ 20-word summary; **keep the entire bullet on ONE line**.\n",
  "• End with the raw URL in parentheses – no markdown.\n\n",
  big_text
)

# ───────── 5.3  call + clean  ──────────────────────────────────────────────
raw <- ask_gpt(prompt1, max_tokens = 700)

clean_gpt_output <- function(txt) {
  # 1️⃣ squash "(url (url))"
  txt <- gsub("\\((https?://[^)\\s]+)\\s*\\(\\1\\)\\)", "(\\1)", txt, perl = TRUE)

  # 2️⃣ drop blank / orphan URL lines
  keep <- function(l) {
    l <- trimws(l)
    !(l == "" || grepl("^https?://", l) || grepl("^\\(", l))
  }
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  paste(lines[vapply(lines, keep, logical(1))], collapse = "\n")
}

fix_parentheses <- function(txt) {
  # turn  ((<https://…>).   →  (<https://…>)
  gsub("\\(\\(<(https?://[^>]+)>\\)[.)]*", "(<\\1>)", txt, perl = TRUE)
}


launches_summary <- raw %>%
  clean_gpt_output() %>%
  # drop any angle brackets the model might have added
  gsub("<(https?://[^>\\s]+)>", "\\1", ., perl = TRUE) %>%
  # if a line ends with a bare URL, wrap it in (...) exactly once
  gsub("(?<!\\))\\s(https?://[^\\s)]+)\\s*$", " (\\1)", ., perl = TRUE)

# finally, replace overall_summary
overall_summary <- launches_summary



# 6 ── SECTION 2 – NUMERIC INSIGHTS, CONTENT TYPE, HASHTAGS ------------------
content_tbl <- df2 |>
  mutate(post_type = tweet_type |> str_to_title()) |>
  group_by(post_type) |>
  summarise(
    avg_ER    = mean(engagement_rate, na.rm = TRUE),
    avg_views = mean(view_count, na.rm = TRUE),
    .groups   = "drop"
  )

content_block <- content_tbl |>
  mutate(row = glue("{post_type}: ER={round(avg_ER,3)}%, views={round(avg_views)}")) |>
  pull(row) |>
  glue_collapse(sep = "\n")

hashtag_block <- df2 |>
  mutate(hashtag = str_extract_all(str_to_lower(text), "#\\w+")) |>
  unnest(hashtag) |>
  group_by(hashtag) |>
  summarise(
    n_tweets = n(),
    avg_ER   = mean(engagement_rate, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  filter(n_tweets >= 3) |>
  arrange(desc(avg_ER)) |>
  slice_head(n = 5) |>
  mutate(row = glue("{hashtag}: ER={round(avg_ER,3)}% (n={n_tweets})")) |>
  pull(row) |>
  glue_collapse(sep = "\n")

num_cols <- c("like_count","retweet_count","reply_count",
              "view_count","engagement_rate")

five_num <- df |>
  summarise(across(all_of(num_cols), \(x){
    q <- quantile(x, c(0,.25,.5,.75,1), na.rm = TRUE)
    glue("min={q[1]}, q1={q[2]}, med={q[3]}, q3={q[4]}, max={q[5]}")
  })) |>
  pivot_longer(everything()) |>
  glue_collapse(sep = "\n")

day_time <- df |>
  mutate(day  = wday(publish_dt, label = TRUE, abbr = FALSE, week_start = 1, locale = "C"),
         hour = hour(publish_dt)) |>
  group_by(day, hour) |>
  summarise(median_engagement = median(engagement_rate, na.rm = TRUE), .groups="drop") |>
  arrange(desc(median_engagement)) |>
  slice_head(n = 10) |>
  mutate(row = glue("{day}: hour={hour}, med_ER={round(median_engagement,3)}")) |>
  pull(row) |>
  glue_collapse(sep = "\n")

prompt2 <- glue(
  "
You are an experienced social‑media analyst.

Each line in **Data A** has `YYYY-MM-DD HH:MM | ER=% | tweet_text`.

**Data B** gives five‑number summaries; **Data C** content types; **Data D** hashtags; **Data E** best‑time.

### Tasks
1. Key Numeric Insights – highest ER, distance to median, tweet text+link, spread/outliers.  
2. Content‑Type Performance – use Data C (give ER & views).  
3. Keyword / Hashtag Trends – 3‑5 terms with higher ER (use Data D).  
4. Best Times to Post – weekdays & 2‑hr windows (use Data E).

**Rules**: bullet points ≤ 12 words; dates `YYYY‑MM‑DD`; don’t invent numbers.

### Data A
{big_text}

### Data B
{five_num}

### Data C
{content_block}

### Data D
{hashtag_block}

### Data E
{day_time}
"
)

overall_summary2 <- ask_gpt(prompt2, max_tokens = 1200)

# 7 ── THEMES BY ENGAGEMENT TIER --------------------------------------------
df_tier <- df |>
  mutate(
    tier = cut(
      engagement_rate,
      breaks = quantile(engagement_rate, c(0,.33,.66,1), na.rm = TRUE),
      labels = c("Low","Medium","High"), include.lowest = TRUE
    )
  )

tidy_tokens <- bind_rows(
  df_tier |> unnest_tokens(word, text),
  df_tier |> unnest_tokens(word, text, token = "ngrams", n = 2) |>
    separate_rows(word, sep = " ")
) |>
  filter(!word %in% c("https","t.co","rt"), !str_detect(word,"^\\d+$")) |>
  anti_join(stop_words, by = "word")

tier_keywords <- tidy_tokens |>
  count(tier, word, sort = TRUE) |>
  bind_tf_idf(word, tier, n) |>
  filter(!is.na(tier)) |>
  group_by(tier) |>
  slice_max(tf_idf, n = 12, with_ties = FALSE) |>
  mutate(row = glue("{word} ({n})")) |>
  summarise(keywords = glue_collapse(row, sep = "; "), .groups = "drop")

prompt3 <- glue(
  "You are a social‑media engagement analyst.\n\n",
  "### Keyword lists\n",
  glue_collapse(sprintf("• %s tier → %s",
                        tier_keywords$tier, tier_keywords$keywords), sep = "\n"),
  "\n\n### Tasks\n",
  "1. For each tier, name the *main theme(s)* in ≤ 100 words.\n",
  "2. Suggest one content strategy to move tweets from Low→Medium and Medium→High."
)

overall_summary3 <- ask_gpt(prompt3, temperature = 0.7, max_tokens = 500)

# 8 ── WEEKLY ROUND‑UP -------------------------------------------------------
start_week <- floor_date(Sys.Date(), "week", week_start = 1)
end_week   <- start_week + days(6)

week_lines <- df |>
  filter(publish_dt >= start_week & publish_dt <= end_week) |>
  mutate(
    line = glue("{format(publish_dt, '%Y-%m-%d %H:%M')} | ",
                "ER={round(engagement_rate,4)}% | ",
                "{str_trunc(text, 200)} | {tweet_url}")
  ) |>
  pull(line)

weekly_prompt <- glue(
  "You are a social‑media analyst.\n\n",
  "Each line in **Data W** is `YYYY-MM-DD HH:MM | ER | snippet | URL`.\n\n",
  "### Tasks\n",
  "1. What happened this week ({start_week}–{end_week}).\n",
  "2. The week ahead (future events/hints).\n",
  "• Bullet points ≤ 15 words; end each point with the URL.\n\n",
  "### Data W\n",
  glue_collapse(week_lines, sep = "\n")
)

overall_summary4 <- ask_gpt(weekly_prompt, temperature = 0.4, max_tokens = 450)

# 9 ── LOCATE CHROME / CHROMIUM ────────────────────────────────────────────
suppressMessages({
  chrome <- Sys.getenv("CHROME_BIN")               # ← 1️⃣ respect env-var first
  if (chrome == "") {
    chrome <- pagedown::find_chrome()              #   2️⃣ fallback search
  }
  if (chrome == "" || !file.exists(chrome)) {
    stop("Cannot find Chrome/Chromium – set CHROME_BIN or install a browser")
  }
  options(pagedown.chromium = chrome)
})


# 10 ── COMBINE, WRITE, RENDER PDF ------------------------------------------
make_md_links <- function(txt) {
  # match a raw URL that is NOT already the target of []()
  pattern <- "(?<!\\]\\()https?://\\S+"

  str_replace_all(
    txt,
    pattern,
    function(m) sprintf("<a href=\"%s\">%s</a>", m, m)  # anchor text = the URL
  )
}


final_report <- glue(
  "{overall_summary}\n\n{overall_summary2}\n\n{overall_summary3}\n\n",
  "## Weekly Round‑up\n\n{overall_summary4}"
) |>
  str_replace_all("\\$", "\\\\$") |>
  make_md_links()

writeLines(c(
  "<style>",
  "  a, a:visited { color:#1a0dab !important; text-decoration:underline; }",
  "</style>",
  "",
  "# Weekly Summary",
  "",
  final_report          # will already contain <a href="…">…</a>
), "summary.md")


# --- Markdown → HTML (no autolink) ---------------------------------
html_file <- tempfile(fileext = ".html")   # ❶ create the temp file FIRST

rmarkdown::pandoc_convert(
  "summary.md",
  to     = "html4",
  from   = "markdown+tex_math_single_backslash",
  output = html_file,                      # ❷ write to it here
  options = c("--standalone","--section-divs","--embed-resources",
              "--variable","bs3=TRUE","--variable","theme=bootstrap")
)

# HTML → PDF  (ONLY this line changes)
pagedown::chrome_print(
  input   = html_file,
  output  = "summary_full.pdf",
  browser = chrome,
  extra_args = c("--headless",     # classic headless
                 "--disable-gpu",
                 "--no-sandbox")
)




# 11 ── UPLOAD TO SUPABASE ---------------------------------------------------
object_path <- sprintf(
  "%s/summary_%s.pdf",
  format(Sys.Date(), "%Yw%V"),
  format(Sys.time(),  "%Y-%m-%d_%H-%M-%S")
)

upload_url <- sprintf(
  "%s/storage/v1/object/%s/%s?upload=1",
  SB_URL, SB_BUCKET, URLencode(object_path, TRUE)
)

request(upload_url) |>
  req_method("POST") |>
  req_headers(
    Authorization = sprintf("Bearer %s", SB_STORAGE_KEY),
    "x-upsert"     = "true",
    "Content-Type" = "application/pdf"
  ) |>
  req_body_file("summary_full.pdf") |>
  req_perform() |>
  resp_check_status()

cat("✔ Uploaded to Supabase:", object_path, "\n")

# 12 ── EMAIL VIA MAILJET ----------------------------------------------------
show_mj_error <- function(resp) {
  cat("\n↪ Mailjet response body:\n",
      resp_body_string(resp, encoding = "UTF-8"), "\n\n")
}

from_email <- if (str_detect(MAIL_FROM, "<.+@.+>")) {
  str_remove_all(str_extract(MAIL_FROM, "<.+@.+>"), "[<>]")
} else {
  MAIL_FROM
}

from_name  <- if (str_detect(MAIL_FROM, "<.+@.+>")) {
  str_trim(str_remove(MAIL_FROM, "<.+@.+>$"))
} else {
  "Report Bot"
}

mj_resp <- request("https://api.mailjet.com/v3.1/send") |>
  req_auth_basic(MJ_API_KEY, MJ_API_SECRET) |>
  req_body_json(list(
    Messages = list(list(
      From      = list(Email = from_email, Name = from_name),
      To        = list(list(Email = MAIL_TO)),
      Subject   = "Weekly Twitter Report",
      TextPart  = "Attached you'll find the weekly report in PDF.",
      Attachments = list(list(
        ContentType   = "application/pdf",
        Filename      = "weekly_report.pdf",
        Base64Content = base64enc::base64encode("summary_full.pdf")
      ))
    ))
  )) |>
  req_error(is_error = \(x) FALSE) |>
  req_perform()

if (resp_status(mj_resp) >= 300) {
  show_mj_error(mj_resp)
  stop(sprintf("Mailjet returned status %s", resp_status(mj_resp)))
} else {
  cat("📧  Mailjet response OK — report emailed\n")
}










