summarize(
  plurality_type = ifelse(sum(get(params$added_feature)) == 0, NA,
                          as.character(names(which.max(table(get(params$added_feature)))))
  ),
  plurality_percentage = ifelse(sum(get(params$added_feature)) == 0, NA,
                                max(get(params$added_feature)) / sum(get(params$added_feature)) * 100
  ),
