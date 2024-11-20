Medflex
================
Sol Libesman

# Checking associations

### Between treatment and mediators

``` r
treatmentmodel <- glm(treatment_cat1  ~ hct_week1_peak+cumalitive_blood_vol_sampled_scaled +arterial_lines+mech_vent_combined,  family =  binomial("logit"), data = final_df)

 glm(treatment_cat1  ~ hct_week1_peak+cumalitive_blood_vol_sampled_scaled +arterial_lines+mech_vent_combined,  family =  binomial("logit"), data = final_df) %>% tbl_regression(exponentiate = TRUE)
```

<div id="pjvcljbsqe" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#pjvcljbsqe table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#pjvcljbsqe thead, #pjvcljbsqe tbody, #pjvcljbsqe tfoot, #pjvcljbsqe tr, #pjvcljbsqe td, #pjvcljbsqe th {
  border-style: none;
}
&#10;#pjvcljbsqe p {
  margin: 0;
  padding: 0;
}
&#10;#pjvcljbsqe .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#pjvcljbsqe .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#pjvcljbsqe .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#pjvcljbsqe .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#pjvcljbsqe .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#pjvcljbsqe .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pjvcljbsqe .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#pjvcljbsqe .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#pjvcljbsqe .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#pjvcljbsqe .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#pjvcljbsqe .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#pjvcljbsqe .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#pjvcljbsqe .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#pjvcljbsqe .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#pjvcljbsqe .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#pjvcljbsqe .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#pjvcljbsqe .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#pjvcljbsqe .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#pjvcljbsqe .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pjvcljbsqe .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#pjvcljbsqe .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#pjvcljbsqe .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#pjvcljbsqe .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pjvcljbsqe .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#pjvcljbsqe .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#pjvcljbsqe .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pjvcljbsqe .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pjvcljbsqe .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#pjvcljbsqe .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#pjvcljbsqe .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#pjvcljbsqe .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pjvcljbsqe .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#pjvcljbsqe .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pjvcljbsqe .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#pjvcljbsqe .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pjvcljbsqe .gt_left {
  text-align: left;
}
&#10;#pjvcljbsqe .gt_center {
  text-align: center;
}
&#10;#pjvcljbsqe .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#pjvcljbsqe .gt_font_normal {
  font-weight: normal;
}
&#10;#pjvcljbsqe .gt_font_bold {
  font-weight: bold;
}
&#10;#pjvcljbsqe .gt_font_italic {
  font-style: italic;
}
&#10;#pjvcljbsqe .gt_super {
  font-size: 65%;
}
&#10;#pjvcljbsqe .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#pjvcljbsqe .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#pjvcljbsqe .gt_indent_1 {
  text-indent: 5px;
}
&#10;#pjvcljbsqe .gt_indent_2 {
  text-indent: 10px;
}
&#10;#pjvcljbsqe .gt_indent_3 {
  text-indent: 15px;
}
&#10;#pjvcljbsqe .gt_indent_4 {
  text-indent: 20px;
}
&#10;#pjvcljbsqe .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;OR&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>OR</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">hct_week1_peak</td>
<td headers="estimate" class="gt_row gt_center">1.03</td>
<td headers="ci" class="gt_row gt_center">1.02, 1.05</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">cumalitive_blood_vol_sampled_scaled</td>
<td headers="estimate" class="gt_row gt_center">1.00</td>
<td headers="ci" class="gt_row gt_center">1.00, 1.01</td>
<td headers="p.value" class="gt_row gt_center">0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">arterial_lines</td>
<td headers="estimate" class="gt_row gt_center">1.42</td>
<td headers="ci" class="gt_row gt_center">1.08, 1.88</td>
<td headers="p.value" class="gt_row gt_center">0.013</td></tr>
    <tr><td headers="label" class="gt_row gt_left">mech_vent_combined</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="ci" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="estimate" class="gt_row gt_center">1.13</td>
<td headers="ci" class="gt_row gt_center">0.78, 1.66</td>
<td headers="p.value" class="gt_row gt_center">0.5</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> OR = Odds Ratio, CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

### global model

examining the relationship between treatment and transfusions (any)
adjusting for all covariates

``` r
adjustedfullmodel <- glm(rc_transfusion_titans ~ treatment_cat1+ hct_week1_peak+cumalitive_blood_vol_sampled_scaled +arterial_lines+mech_vent_combined +GA_weeks_and_days_integer + multiple,  family =  binomial("logit"), data = final_df)


glm(rc_transfusion_titans ~ treatment_cat1+ hct_week1_peak+cumalitive_blood_vol_sampled_scaled +arterial_lines+mech_vent_combined +GA_weeks_and_days_integer + multiple,  family =  binomial("logit"), data = final_df) %>% tbl_regression(exponentiate = TRUE)
```

<div id="ebvsdzkodb" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ebvsdzkodb table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ebvsdzkodb thead, #ebvsdzkodb tbody, #ebvsdzkodb tfoot, #ebvsdzkodb tr, #ebvsdzkodb td, #ebvsdzkodb th {
  border-style: none;
}
&#10;#ebvsdzkodb p {
  margin: 0;
  padding: 0;
}
&#10;#ebvsdzkodb .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ebvsdzkodb .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ebvsdzkodb .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ebvsdzkodb .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ebvsdzkodb .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ebvsdzkodb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ebvsdzkodb .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ebvsdzkodb .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ebvsdzkodb .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ebvsdzkodb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ebvsdzkodb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ebvsdzkodb .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ebvsdzkodb .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ebvsdzkodb .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ebvsdzkodb .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ebvsdzkodb .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ebvsdzkodb .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ebvsdzkodb .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ebvsdzkodb .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ebvsdzkodb .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ebvsdzkodb .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ebvsdzkodb .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ebvsdzkodb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ebvsdzkodb .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ebvsdzkodb .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ebvsdzkodb .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ebvsdzkodb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ebvsdzkodb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ebvsdzkodb .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ebvsdzkodb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ebvsdzkodb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ebvsdzkodb .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ebvsdzkodb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ebvsdzkodb .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ebvsdzkodb .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ebvsdzkodb .gt_left {
  text-align: left;
}
&#10;#ebvsdzkodb .gt_center {
  text-align: center;
}
&#10;#ebvsdzkodb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ebvsdzkodb .gt_font_normal {
  font-weight: normal;
}
&#10;#ebvsdzkodb .gt_font_bold {
  font-weight: bold;
}
&#10;#ebvsdzkodb .gt_font_italic {
  font-style: italic;
}
&#10;#ebvsdzkodb .gt_super {
  font-size: 65%;
}
&#10;#ebvsdzkodb .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ebvsdzkodb .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ebvsdzkodb .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ebvsdzkodb .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ebvsdzkodb .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ebvsdzkodb .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ebvsdzkodb .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;OR&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>OR</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">treatment_cat1</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="ci" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="estimate" class="gt_row gt_center">0.73</td>
<td headers="ci" class="gt_row gt_center">0.53, 1.00</td>
<td headers="p.value" class="gt_row gt_center">0.053</td></tr>
    <tr><td headers="label" class="gt_row gt_left">hct_week1_peak</td>
<td headers="estimate" class="gt_row gt_center">0.94</td>
<td headers="ci" class="gt_row gt_center">0.92, 0.96</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">cumalitive_blood_vol_sampled_scaled</td>
<td headers="estimate" class="gt_row gt_center">1.07</td>
<td headers="ci" class="gt_row gt_center">1.05, 1.08</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">arterial_lines</td>
<td headers="estimate" class="gt_row gt_center">1.16</td>
<td headers="ci" class="gt_row gt_center">0.83, 1.64</td>
<td headers="p.value" class="gt_row gt_center">0.4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">mech_vent_combined</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="ci" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="estimate" class="gt_row gt_center">1.76</td>
<td headers="ci" class="gt_row gt_center">1.04, 3.05</td>
<td headers="p.value" class="gt_row gt_center">0.040</td></tr>
    <tr><td headers="label" class="gt_row gt_left">GA_weeks_and_days_integer</td>
<td headers="estimate" class="gt_row gt_center">0.93</td>
<td headers="ci" class="gt_row gt_center">0.92, 0.94</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">multiple</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="ci" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="estimate" class="gt_row gt_center">1.41</td>
<td headers="ci" class="gt_row gt_center">0.99, 2.03</td>
<td headers="p.value" class="gt_row gt_center">0.058</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> OR = Odds Ratio, CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
unadjustedmodel <- glm(rc_transfusion_titans ~ treatment_cat1,  family =  binomial("logit"), data = final_df) %>%
  tbl_regression(exponentiate = TRUE,
                 show_single_row="treatment_cat1",
                 include = c("treatment_cat1"))
```

``` r
adjusted_basedlinedmodel <- glm(rc_transfusion_titans ~ treatment_cat1+GA_weeks_and_days_integer + multiple,  family =  binomial("logit"), data = final_df)%>%  
  tbl_regression(exponentiate = TRUE,
                 show_single_row="treatment_cat1",
                 include = c("treatment_cat1"))
```

``` r
adjusted_hct_mediator_model <- glm(rc_transfusion_titans ~ treatment_cat1+ hct_week1_peak +GA_weeks_and_days_integer + multiple,  family =  binomial("logit"), data = final_df) %>%   
  tbl_regression(exponentiate = TRUE,
                 show_single_row="treatment_cat1",
                 include = c("treatment_cat1"))
```

``` r
adjusted_severity_of_illness_mediator_model <- glm(rc_transfusion_titans ~ treatment_cat1+ cumalitive_blood_vol_sampled_scaled +arterial_lines+mech_vent_combined +GA_weeks_and_days_integer + multiple,  family =  binomial("logit"), data = final_df) %>%   tbl_regression(exponentiate = TRUE,
                 show_single_row="treatment_cat1",
                 include = c("treatment_cat1"))
```

``` r
adjustedfullmodel <- glm(rc_transfusion_titans ~ treatment_cat1+ hct_week1_peak+cumalitive_blood_vol_sampled_scaled +arterial_lines+mech_vent_combined +GA_weeks_and_days_integer + multiple,  family =  binomial("logit"), data = final_df) %>%   tbl_regression(exponentiate = TRUE,
                 show_single_row="treatment_cat1",
                 include = c("treatment_cat1"))
```

``` r
models.a<-tbl_stack(list(unadjustedmodel, 
                         adjusted_basedlinedmodel, 
                         adjusted_hct_mediator_model,
                         adjusted_severity_of_illness_mediator_model,
                         adjustedfullmodel))

models.a$table_body %>%
  mutate(name = case_when(tbl_id1==1 ~ "Unadjusted", 
                          tbl_id1==2 ~ "Adjusted for baseline covariates" , 
                          tbl_id1==3 ~ "Hct Mediation adjusted",
                          tbl_id1==4 ~ "Severity of illness Mediation adjusted",
                          tbl_id1==5 ~ "Global model",) %>%
           as.factor()) %>%
  ggplot(aes(y=fct_reorder(name, -tbl_id1), x=estimate)) +
  geom_point(size=3)+
  geom_errorbar(aes(xmax=conf.high, xmin=conf.low), size=0.5, width=0.1) +
  labs(x="Odds ratio (95% CI)", y="")+
  geom_vline(aes(xintercept= 1), linetype="dotted")+
  coord_trans(x = scales:::log_trans(base = exp(1))) +
  theme_minimal() -> plot.a

plot.a
```

<img src="Analysis_medflex_for_Git2_files/figure-gfm/unnamed-chunk-10-1.png" width="100%" />

# Medflex (sequential approach)

<https://academic.oup.com/ije/article/47/3/829/4829681?login=true>

kristy paper
<https://academic.oup.com/ejendo/article/189/1/50/7219871?login=true>
<https://github.com/kristyrobledo/T4DM_mediation_paper>

instruction paper
<https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-022-01764-w>

(sup materials)
<https://static-content.springer.com/esm/art%3A10.1186%2Fs12874-022-01764-w/MediaObjects/12874_2022_1764_MOESM1_ESM.pdf>

## Outcome: transufsion any (y/n)

### Mediation of Hct peak (M1 only)

``` r
final_df1 <- final_df %>% filter(!is.na(rc_transfusion_titans) &
                                  !is.na(hct_week1_peak)&
                                   !is.na(GA_weeks_and_days_integer)&
                                  !is.na(multiple))


impData <- medflex::neImpute(rc_transfusion_titans ~ factor(treatment_cat1) +
                     hct_week1_peak +
                    GA_weeks_and_days_integer +
                    multiple,
                    family = binomial("logit"), nMed = 1, data = final_df1)


#head(impData)
neMod.extra.cont <- medflex::neModel(rc_transfusion_titans ~ treatment_cat10+
                              treatment_cat11 +
                    GA_weeks_and_days_integer +
                    multiple,
                  family = binomial("logit"),
                  expData = impData,
                  se = "robust")


#summary(neMod.extra.cont)

cont.extra<-data.frame(est = neMod.extra.cont$neModelFit$coefficients, confint(neMod.extra.cont))

lht <- medflex::neLht(neMod.extra.cont, linfct = c("treatment_cat101 = 0", 
                                          "treatment_cat111  = 0", 
                                          "treatment_cat101 + treatment_cat111  = 0"))

t<-summary(lht)

cat.extra<-data.frame(est = exp(t$coefficients[,1]), exp(confint(lht)))

#t


#pte.direct.cat<-(t$coefficients[1,1]/t$coefficients[3,1])*100
pte.indirect.cat<-(t$coefficients[2,1]/t$coefficients[3,1])*100
#pte.indirect.cat


cat.extra %>%
  mutate(name = c("Direct effect", "Indirect effect", "Total effect")) %>%
  ggplot(aes(y=fct_rev(name), x=est)) +
  geom_point(size=3)+
  geom_errorbar(aes(xmax=X95..UCL, xmin=X95..LCL), size=0.5, width=0.1) +
  labs(x="Odds ratio (95% CI)", y="")+
  geom_vline(aes(xintercept= 1), linetype="dotted")+
  coord_trans(x = scales:::log_trans(base = exp(1))) +
  theme_minimal() -> plot.a2

plot.a2
```

<img src="Analysis_medflex_for_Git2_files/figure-gfm/unnamed-chunk-11-1.png" width="100%" />

``` r
#Formulat for table
cat.extra <- cat.extra %>%
  mutate(Effect = c("Direct effect", "Indirect effect", "Total effect")) %>% relocate(Effect) %>% dplyr::rename(
    "Estimate"= est,
    "95%_CI_L"= `X95..LCL`,
   "95%_CI_U"= `X95..UCL`,)
row.names(cat.extra) <- NULL





M1.model_transfusion_any <- cat.extra


M1.model_transfusion_any #%>% kable() %>% kable_styling()
```

    #>            Effect  Estimate  95%_CI_L  95%_CI_U
    #> 1   Direct effect 0.7646152 0.5842532 1.0006559
    #> 2 Indirect effect 0.8535287 0.7870670 0.9256026
    #> 3    Total effect 0.6526210 0.4973579 0.8563536

``` r
prop_mediated_M1.model_trans_any <-  round((t$coef[3]/(t$coef[2]+t$coef[3])*100),1)
```

``` r
print(paste0("Proportion mediated: ",prop_mediated_M1.model_trans_any , "%"))
```

    #> [1] "Proportion mediated: 37.1%"

## Joint model including both Hct (M1) and Severity of illness (M2)

``` r
final_df1 <- final_df %>% filter(!is.na(hct_week1_peak)&
                                  !is.na(cumalitive_blood_vol_sampled)&
                                  !is.na(arterial_lines)&
                                  !is.na(mech_vent_combined)&
                                  !is.na(GA_weeks_and_days_integer)&
                                  !is.na(multiple))


#==========================================================================================================
impData <- medflex::neImpute(rc_transfusion_titans ~ factor(treatment_cat1) +
                     hct_week1_peak +cumalitive_blood_vol_sampled +  arterial_lines+ mech_vent_combined +
                    GA_weeks_and_days_integer +
                    multiple,
                    family = binomial("logit"), nMed = 4, data = final_df1)


#head(impData)
neMod.extra.cont <- medflex::neModel(rc_transfusion_titans ~ treatment_cat10+
                              treatment_cat11+
                    GA_weeks_and_days_integer +
                    multiple,
                  family = binomial("logit"),
                  expData = impData,
                  se = "robust")


#summary(neMod.extra.cont)

cont.extra<-data.frame(est = neMod.extra.cont$neModelFit$coefficients, confint(neMod.extra.cont))

lht <- medflex::neLht(neMod.extra.cont, linfct = c("treatment_cat101 = 0", 
                                          "treatment_cat111  = 0", 
                                          "treatment_cat101 + treatment_cat111  = 0"))

t<-summary(lht)

cat.extra<-data.frame(est = exp(t$coefficients[,1]), exp(confint(lht)))

#t


#pte.direct.cat<-(t$coefficients[1,1]/t$coefficients[3,1])*100
pte.indirect.cat<-(t$coefficients[2,1]/t$coefficients[3,1])*100
#pte.indirect.cat


cat.extra %>%
  mutate(name = c("Direct effect", "Indirect effect", "Total effect")) %>%
  ggplot(aes(y=fct_rev(name), x=est)) +
  geom_point(size=3)+
  geom_errorbar(aes(xmax=X95..UCL, xmin=X95..LCL), size=0.5, width=0.1) +
  labs(x="Odds ratio (95% CI)", y="")+
  geom_vline(aes(xintercept= 1), linetype="dotted")+
  coord_trans(x = scales:::log_trans(base = exp(1))) +
  theme_minimal() -> plot.a2

plot.a2
```

<img src="Analysis_medflex_for_Git2_files/figure-gfm/unnamed-chunk-14-1.png" width="100%" />

``` r
#code to table results
cat.extra <- cat.extra %>%
  mutate(Effect = c("Direct effect", "Indirect effect", "Total effect")) %>% relocate(Effect) %>% dplyr::rename(
    "Estimate"= est,
    "95%_CI_L"= `X95..LCL`,
   "95%_CI_U"= `X95..UCL`)
row.names(cat.extra) <- NULL

#cat.extra


joint.M1M2.model_transfusion_any <- cat.extra
joint.M1M2.model_transfusion_any# %>% kable() %>% kable_styling()
```

    #>            Effect  Estimate  95%_CI_L  95%_CI_U
    #> 1   Direct effect 0.7636927 0.5800280 1.0055144
    #> 2 Indirect effect 0.9072675 0.8029812 1.0250978
    #> 3    Total effect 0.6928736 0.5162991 0.9298366

``` r
prop_mediated_joint_model_trans_any <-  round((t$coef[3]/(t$coef[2]+t$coef[3])*100),1)
```

``` r
paste0("Proportion mediated: ",prop_mediated_joint_model_trans_any, "%")
```

    #> [1] "Proportion mediated: 26.5%"

# Table of results

``` r
M1_model_results <- bind_rows(list("M1 only: transfusion_any"=M1.model_transfusion_any), .id="id")
M1_model_results <- M1_model_results %>% mutate(across(c(Estimate, `95%_CI_L`, `95%_CI_U`), round, digits=2) )
```

    #> Warning: There was 1 warning in `mutate()`.
    #> ℹ In argument: `across(c(Estimate, `95%_CI_L`, `95%_CI_U`), round, digits =
    #>   2)`.
    #> Caused by warning:
    #> ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
    #> Supply arguments directly to `.fns` through an anonymous function instead.
    #> 
    #>   # Previously
    #>   across(a:b, mean, na.rm = TRUE)
    #> 
    #>   # Now
    #>   across(a:b, \(x) mean(x, na.rm = TRUE))

``` r
M1_model_results$est_for_table <- paste0(M1_model_results$Estimate, " (", M1_model_results$`95%_CI_L`,"-", M1_model_results$`95%_CI_U`,")")

M1_model_results <- M1_model_results %>% select(id, Effect, est_for_table)


M1M2_joint_model_results <- bind_rows(list("M1M2 joint model: transfusion_any"=joint.M1M2.model_transfusion_any), .id="id")
M1M2_joint_model_results <- M1M2_joint_model_results %>% mutate(across(c(Estimate, `95%_CI_L`, `95%_CI_U`), round, digits=2) )
M1M2_joint_model_results$est_for_table <- paste0(M1M2_joint_model_results$Estimate, " (", M1M2_joint_model_results$`95%_CI_L`,"-", M1M2_joint_model_results$`95%_CI_U`,")")
M1M2_joint_model_results <- M1M2_joint_model_results %>% select(id, Effect, est_for_table)


combined_mediation_results <- cbind(M1_model_results, M1M2_joint_model_results)



combined_mediation_results %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
id
</th>
<th style="text-align:left;">
Effect
</th>
<th style="text-align:left;">
est_for_table
</th>
<th style="text-align:left;">
id
</th>
<th style="text-align:left;">
Effect
</th>
<th style="text-align:left;">
est_for_table
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
M1 only: transfusion_any
</td>
<td style="text-align:left;">
Direct effect
</td>
<td style="text-align:left;">
0.76 (0.58-1)
</td>
<td style="text-align:left;">
M1M2 joint model: transfusion_any
</td>
<td style="text-align:left;">
Direct effect
</td>
<td style="text-align:left;">
0.76 (0.58-1.01)
</td>
</tr>
<tr>
<td style="text-align:left;">
M1 only: transfusion_any
</td>
<td style="text-align:left;">
Indirect effect
</td>
<td style="text-align:left;">
0.85 (0.79-0.93)
</td>
<td style="text-align:left;">
M1M2 joint model: transfusion_any
</td>
<td style="text-align:left;">
Indirect effect
</td>
<td style="text-align:left;">
0.91 (0.8-1.03)
</td>
</tr>
<tr>
<td style="text-align:left;">
M1 only: transfusion_any
</td>
<td style="text-align:left;">
Total effect
</td>
<td style="text-align:left;">
0.65 (0.5-0.86)
</td>
<td style="text-align:left;">
M1M2 joint model: transfusion_any
</td>
<td style="text-align:left;">
Total effect
</td>
<td style="text-align:left;">
0.69 (0.52-0.93)
</td>
</tr>
</tbody>
</table>
