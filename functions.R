html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

wday_sun <- function(d) lubridate::wday(d, week_start = 7) # 1 = Sun ... 7 = Sat


get_meta <- function(sport) {
  m <- sport_meta[[sport]]
  if (is.null(m)) {
    list(icon = "&bull;", label = sport, class = "sport-other")
  } else {
    m
  }
}

build_day_cell <- function(d) {
  if (is.na(d)) {
    return('<div class="day-cell empty"></div>')
  }

  day_workouts <- workouts |> dplyr::filter(date == d)
  day_num <- as.integer(format(d, "%d"))
  cls <- c("day-cell")
  if (d == Sys.Date()) {
    cls <- c(cls, "today")
  }
  if (wday_sun(d) %in% c(1, 7)) {
    cls <- c(cls, "weekend")
  }

  if (nrow(day_workouts) == 0) {
    chips <- ""
  } else {
    chip_list <- vapply(
      seq_len(nrow(day_workouts)),
      function(i) {
        w <- day_workouts[i, ]
        meta <- get_meta(w$sport)
        done <- if (isTRUE(w$completed)) "completed" else ""
        title_e <- html_escape(w$title)
        note_e <- if (is.na(w$notes) || w$notes == "") {
          ""
        } else {
          paste0(" &mdash; ", html_escape(w$notes))
        }
        tooltip <- paste0(title_e, " &middot; ", w$duration_min, " min", note_e)
        paste0(
          '<div class="chip ',
          meta$class,
          ' ',
          done,
          '" title="',
          tooltip,
          '">',
          '<span class="chip-icon">',
          meta$icon,
          '</span>',
          '<span class="chip-label">',
          title_e,
          '</span>',
          '</div>'
        )
      },
      character(1)
    )
    chips <- paste(chip_list, collapse = "")
  }

  paste0(
    '<div class="',
    paste(cls, collapse = " "),
    '">',
    '<div class="day-num">',
    day_num,
    '</div>',
    '<div class="chip-stack">',
    chips,
    '</div>',
    '</div>'
  )
}