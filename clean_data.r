# load environment ####
invisible({
  pkgs = c('dplyr', 'reshape2', 'stringr', 'tidyr', 'xlsx')
  lapply(pkgs, library, character.only = TRUE)
})

# functions ####
# day means
SplitDay = function(cols, month, df, type) {
  df = df %>%
    select(all_of(cols)) %>%
    drop_na()
  if (type == 'vento') {
    vars = c('vento_dir', 'vento_med', 'vento_max')
    vento_dirs = seq(0, 360 - 22.5, 22.5)
    names(vento_dirs) = c('N', 'NNE', 'NE', 'ENE',
                          'E', 'ESE', 'SE', 'SSE',
                          'S', 'SSW', 'SW', 'WSW',
                          'W', 'WNW', 'NW', 'NNW')
    df = mutate_at(df, 1, function(x) as.vector(vento_dirs[str_trim(x)]))
  } else {
    vars = type
  }
  df = df %>%
    'colnames<-'(vars) %>%
    mutate(month = month, day = 1:n())
  return(df)
}
ExtractDay = function(year, data_path) {
  type = str_extract(data_path, 'insol|irrad|vento')
  start = ifelse(type == 'vento', 10, 8)
  end = ifelse(type == 'vento', 40, 38)
  df = data_path %>%
    read.xlsx(sheetName = as.character(year), header = FALSE,
              startRow = start, endRow = end)
  if (type == 'vento') {
    cols = lapply(0:11, function(x) 2:4 + x*5)
    df = mapply(SplitDay, cols, 1:12, SIMPLIFY = FALSE, MoreArgs = list(df, type))
  } else {
    df = mapply(SplitDay, 3:14, 1:12, SIMPLIFY = FALSE, MoreArgs = list(df, type))
  }
  df = df %>%
    bind_rows() %>%
    mutate(year = year) %>%
    select(c('year', 'month', 'day', everything()))
  return(df)
}
CleanDay = function(data_path) {
  df = 1986:2007 %>%
    lapply(ExtractDay, data_path) %>%
    bind_rows()
  return(df)
}
# hour means
SplitHour = function(month, file_path, var) {
  df = file_path %>%
    read.xlsx(month, startRow = 4, endRow = 27, header = FALSE, colIndex = 1:32) %>%
    select_if(function(x) sum(is.na(x)) != 24) %>%
    melt(id.vars = 1) %>%
    mutate(variable = as.numeric(str_remove(variable, '\\D')) - 1,
           month = month) %>%
    'colnames<-'(c('hour', 'day', var, 'month')) %>%
    select(c('month', 'day', 'hour', all_of(var)))
  pos = which(is.na(df[[var]]))
  df[[var]][pos] = mean(c(df[[var]][pos - 1], df[[var]][pos + 1]))
  return(df)
}
ExtractHour = function(file_path, var) {
  df = 1:12 %>%
    lapply(SplitHour, file_path, var) %>%
    bind_rows()
  return(df)
}
CleanHour = function(var, input_dir) {
  pattern = paste0('^', var, '_')
  df2 = input_dir %>%
    dir(pattern, full.names = TRUE) %>%
    lapply(ExtractHour, var) %>%
    bind_rows()
}
# bind data
BindData = function(level, input_dir) {
  if (level == 'day') {
    vars = str_flatten(c('vento', 'irrad', 'insol'), '|')
    data_paths = dir(input_dir, vars, full.names = TRUE)
    data = lapply(data_paths, CleanDay)
  } else {
    vars = c('po', 'press', 'temp', 'ue', 'ur')
    data = lapply(vars, CleanHour, input_dir)
  }
  data = Reduce(function(x, y) merge(x, y, all = TRUE), data)
  write.csv(data, paste0(input_dir, 'data_', level, '.csv'), row.names = FALSE)
}

# application ####
lapply(c('day', 'hour'), BindData, './data/')
