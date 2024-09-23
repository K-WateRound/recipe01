# ------------------------------------------------------------------------------------
# 번  호: 1
# 함수명: ts_graph_fn
# 입력값: input_df (그래프 작성을 위한 데이터), input.var(날짜 변수명), input.scales (축 범위 고정 혹은 변동)
# 설  명: ggplot을 이용한 시계열 그래프(line graph) 작성
# ------------------------------------------------------------------------------------

ts_graph_fn <- function(input_df, input.var = "DATE", input.scales = c('free_x', 'free'), nrow = NULL, ncol = NULL) {
  # 그래프 작성을 위한 데이터 변환
  sel_reshape_df <- reshape(data = input_df, idvar = input.var,
                            varying = names(input_df)[-1],
                            v.name = c('Value'),
                            times = names(input_df)[-1],
                            direction = 'long')
  names(sel_reshape_df) <- c('Date', 'Variable', 'Value')
  row.names(sel_reshape_df) <- NULL
  
  # ggplot을 이용한 시계열 그래프 작성
  date.var <- "Date"
  var.x <- 'Value'
  group.var <- 'Variable'

  sel_reshape_df %>%
    ggplot() + 
    geom_line(aes_string(x = date.var, y = var.x)) +
    facet_wrap(facets = as.formula(paste('~', group.var)), scales = input.scales, labeller = label_both, nrow = nrow, ncol = ncol) +
    theme(strip.text.x = element_text(size = 16), axis.title = element_text(size = 16), axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
    scale_x_datetime(labels = date_format("%y-%m-%d %H:%M"))
}