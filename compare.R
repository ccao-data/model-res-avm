all.equal(
  data.frame(assessment_card_data_pred),
  data.frame(assessment_card_data_pred_2)
)

all.equal(
  data.frame(assessment_card_data_mc),
  data.frame(assessment_card_data_mc_2)
)

all.equal(
  data.frame(assessment_card_data_cid),
  data.frame(assessment_card_data_cid_2)
)

all.equal(
  data.frame(assessment_card_data_round),
  data.frame(assessment_card_data_round_2)
)

all.equal(
  data.frame(assessment_pin_data_w_land),
  data.frame(assessment_pin_data_w_land_2)
)

all.equal(
  data.frame(assessment_pin_data_prorated),
  data.frame(assessment_pin_data_prorated_2)
)

all.equal(
  data.frame(assessment_card_data_merged),
  data.frame(assessment_card_data_merged_2)
)

all.equal(
  data.frame(sales_data_ratio_study),
  data.frame(sales_data_ratio_study_2)
)

all.equal(
  data.frame(sales_data_two_most_recent),
  data.frame(sales_data_two_most_recent_2)
)

all.equal(
  data.frame(assessment_pin_data_base),
  data.frame(assessment_pin_data_base_2 %>% arrange(meta_year, meta_pin))
)

all.equal(
  data.frame(assessment_pin_data_sale),
  data.frame(assessment_pin_data_sale_2 %>% arrange(meta_year, meta_pin))
)

all.equal(
  data.frame(assessment_pin_data_final),
  data.frame(assessment_pin_data_final_2 %>% arrange(meta_year, meta_pin))
)
