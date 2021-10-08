library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-09-14')
library(tidyverse)
library(dplyr)
tuesdata$billboard->billboard
tuesdata$audio_features->audio
library(gtExtras)
library(gt)
library(emojifont)

glimpse(billboard)
glimpse(audio)

billboard%>%left_join(audio, by="song_id")->song
lubridate::mdy(song$week_id)->song$week_id

song%>%filter(performer.x=="Taylor Swift")->ts
ts%>%filter(peak_position==1)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%group_by(song.x)%>%
  slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->tssongs
tssongs

song%>%filter(performer.x=="Billie Eilish")->be
be%>%filter(peak_position==1)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%group_by(song.x)%>%
  slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->besongs
besongs

song%>%filter(performer.x=="Megan Thee Stallion")->mts
mts%>%filter(peak_position==1)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%group_by(song.x)%>%
  slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->mtssongs
mtssongs

song%>%filter(performer.x=="Lizzo")->l
l%>%filter(peak_position==1)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%group_by(song.x)%>%
  slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->lsongs
lsongs

song%>%filter(performer.x=="Ariana Grande")->ag
ag%>%filter(peak_position==1)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%group_by(song.x)%>%
  slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->agsongs
agsongs


song%>%filter(performer.x=="Halsey")->h
h%>%filter(peak_position==1)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%group_by(song.x)%>%
  slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->hsongs
hsongs

rbind(tssongs,besongs,mtssongs,lsongs,agsongs,hsongs)->songf
data.frame(songf)->songf
songf1%>%gt()%>%gt_theme_espn()
songf[c(1,2,4)]->songf1
songf1%>%arrange(desc('NUMBER OF WEEKS AS #1'))
songf1[order(songf1$`NUMBER OF WEEKS AS #1`, decreasing = TRUE),]->songf1
colnames(songf1)<-c("SINGER","SONG","NUMBER OF WEEKS AS #1")
typeof(songf1$`NUMBER OF WEEKS AS #1`)

songf1%>%gt()%>%gt_hulk_col_numeric(`NUMBER OF WEEKS AS #1`)
songf1%>%mutate("BAR"= round("NUMBER OF WEEKS AS #1"))%>%
  gt()%>% gt_plt_bar(column = `BAR`) %>%
    cols_width(3 ~ px(125))

songf1%>%mutate(BAROMETER=round(`NUMBER OF WEEKS AS #1`))%>%
  gt()%>% gt_plt_bar(column = BAROMETER, fill = "#2ab7ca", background = "#cccccc") %>%
  cols_width(4 ~ px(125))%>%
  tab_style(
    style = list(
      cell_fill(color = "#ec4567"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      rows = SINGER=="Taylor Swift")
  )%>%
  tab_style(
    style = list(
      cell_fill(color = "#39316f"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      rows = SINGER=="Halsey")
  )%>%
  tab_style(
    style = list(
      cell_fill(color = "#005b96"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      rows = SINGER=="Billie Eilish")
  )%>%
  tab_style(
    style = list(
      cell_fill(color = "#3a163a"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      rows = SINGER=="Lizzo")
  )%>%
  tab_style(
    style = list(
      cell_fill(color = "#000000"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      rows = SINGER=="Ariana Grande")
  )%>%
  tab_style(
    style = list(
      cell_fill(color = "#00225c"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      rows = SINGER=="Megan Thee Stallion")
  )%>%
  tab_header(
    title = md(paste0("#1 SONGS OF 2020'S TOP 10 FEMALE BILLBOARD ARTISTS ",
                      emoji("musical_note"),
               emoji("musical_score"))),
    subtitle = "These songs have been ranked #1 on Billboard and have been arranged based on the number of weeks they were in the spot"
  ) %>%
  tab_source_note(md("Data: Data.World via Sean Miller|Design and Analysis: @annapurani93"))%>%
  tab_style(
    style = cell_text(
      align = "right"
    ),
    locations = cells_source_notes()
  )%>%
  tab_style(style = list(cell_text(align="center"), table.align="center",heading.align = "center"), locations=list(cells_body(columns = everything(),rows = everything())))%>%
  cols_align(
    align = "center",
    columns = everything())%>%
  tab_options(
    source_notes.background.color = "#2a4d69",
    heading.background.color = "#2a4d69",
    column_labels.background.color = "#aaaaaa",
    table_body.hlines.color = "#989898",
    table_body.border.top.color = "#989898",
    heading.border.bottom.color = "#989898",
    row_group.border.top.color = "#989898",
    summary_row.border.color = "#989898"
    )%>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        color = "#989898",
        weight = px(1),
        style="dashed"
      ),
      cell_borders(
        sides = c("left", "right"),
        color = "#989898",
        weight = px(1),
        style="dashed"
      )),
        locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  )->table
  
table



