Medflex
================
Sol Libesman


    # Checking associations

    ### Between treatment and mediators



    ```r
    treatmentmodel <- glm(treatment_cat1  ~ hct_week1_peak+cumalitive_blood_vol_sampled_scaled +arterial_lines+mech_vent_combined,  family =  binomial("logit"), data = final_df)

    treatmentmodel2 <- treatmentmodel %>% tbl_regression(exponentiate = TRUE)

    as_kable(treatmentmodel2, format = "markdown")

| **Characteristic**                  | **OR** | **95% CI** | **p-value** |
|:------------------------------------|:------:|:----------:|:-----------:|
| hct_week1_peak                      |  1.03  | 1.02, 1.05 |   \<0.001   |
| cumalitive_blood_vol_sampled_scaled |  1.00  | 1.00, 1.01 |     0.8     |
| arterial_lines                      |  1.42  | 1.08, 1.88 |    0.013    |
| mech_vent_combined                  |        |            |             |
| 0                                   |   —    |     —      |             |
| 1                                   |  1.11  | 0.76, 1.62 |     0.6     |

### global model

examining the relationship between treatment and transfusions (any)
adjusting for all covariates

``` r
adjustedfullmodel <- glm(rc_transfusion_titans ~ treatment_cat1+ hct_week1_peak+cumalitive_blood_vol_sampled_scaled +arterial_lines+mech_vent_combined +GA_weeks_and_days_integer + multiple,  family =  binomial("logit"), data = final_df) %>%  tbl_regression(exponentiate = TRUE,
                 show_single_row="treatment_cat1",
                 include = c("treatment_cat1"))
```

``` r
#unadjustedmodel <- glm(rc_transfusion_titans ~ treatment_cat1,  family =  binomial("logit"), data = final_df) %>%
#  tbl_regression(exponentiate = TRUE,
#                 show_single_row="treatment_cat1",
#                include = c("treatment_cat1"))
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
models.a<-tbl_stack(list( adjusted_basedlinedmodel, 
                         adjusted_hct_mediator_model,
                         adjusted_severity_of_illness_mediator_model,
                         adjustedfullmodel))

models.a$table_body %>%
  mutate(name = case_when(tbl_id1==1 ~ "Adjusted for baseline covariates" , 
                          tbl_id1==2 ~ "Hct Mediation adjusted",
                          tbl_id1==3 ~ "Severity of illness Mediation adjusted",
                          tbl_id1==4 ~ "Global model with Hct, severity of illness and covariates",) %>%
           as.factor()) %>%
  ggplot(aes(y=fct_reorder(name, -tbl_id1), x=estimate)) +
  geom_point(size=3)+
  geom_errorbar(aes(xmax=conf.high, xmin=conf.low), size=0.5, width=0.1) +
  labs(x="Odds ratio of treatment effect on transfusion any (y/n)", y="")+
  geom_vline(aes(xintercept= 1), linetype="dotted")+
  coord_trans(x = scales:::log_trans(base = exp(1))) +
  theme_minimal() -> plot.a

plot.a
```

<img src="Analysis_medflex_for_Git2_files/figure-gfm/unnamed-chunk-10-1.png" width="100%" />

# Medflex (sequential approach)

Modeling approach informed by

kristy Robledo’s paper
<https://academic.oup.com/ejendo/article/189/1/50/7219871?login=true>
<https://github.com/kristyrobledo/T4DM_mediation_paper>

Zoe Aitken’s paper on sequential mediation
<https://academic.oup.com/ije/article/47/3/829/4829681?login=true>

and D Zugna’s instruction paper
<https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-022-01764-w>
(sup materials)
<https://static-content.springer.com/esm/art%3A10.1186%2Fs12874-022-01764-w/MediaObjects/12874_2022_1764_MOESM1_ESM.pdf>

## Outcome: transufsion any (y/n)

### Mediation of Hct peak (M1 only)

First we examine whether Hct alone mediates the effect of intervention
of outcome

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

neMod.extra.cont <- medflex::neModel(rc_transfusion_titans ~ treatment_cat10+
                              treatment_cat11 +
                    GA_weeks_and_days_integer +
                    multiple,
                  family = binomial("logit"),
                  expData = impData,
                  se = "robust")


cont.extra<-data.frame(est = neMod.extra.cont$neModelFit$coefficients, confint(neMod.extra.cont))

lht <- medflex::neLht(neMod.extra.cont, linfct = c("treatment_cat101 = 0", 
                                          "treatment_cat111  = 0", 
                                          "treatment_cat101 + treatment_cat111  = 0"))

t<-summary(lht)

cat.extra<-data.frame(est = exp(t$coefficients[,1]), exp(confint(lht)))

pte.indirect.cat<-(t$coefficients[2,1]/t$coefficients[3,1])*100


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

M1.model_transfusion_any %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Effect
</th>
<th style="text-align:right;">
Estimate
</th>
<th style="text-align:right;">
95%\_CI_L
</th>
<th style="text-align:right;">
95%\_CI_U
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Direct effect
</td>
<td style="text-align:right;">
0.7601151
</td>
<td style="text-align:right;">
0.5811334
</td>
<td style="text-align:right;">
0.9942208
</td>
</tr>
<tr>
<td style="text-align:left;">
Indirect effect
</td>
<td style="text-align:right;">
0.8540546
</td>
<td style="text-align:right;">
0.7876051
</td>
<td style="text-align:right;">
0.9261104
</td>
</tr>
<tr>
<td style="text-align:left;">
Total effect
</td>
<td style="text-align:right;">
0.6491798
</td>
<td style="text-align:right;">
0.4951578
</td>
<td style="text-align:right;">
0.8511112
</td>
</tr>
</tbody>
</table>

``` r
prop_mediated_M1.model_trans_any <-  round((t$coef[3]/(t$coef[2]+t$coef[3])*100),0)

paste0("Proprtion mediated: ", prop_mediated_M1.model_trans_any ," (%)") %>% kable(col.names=NULL)
```

<table>
<tbody>
<tr>
<td style="text-align:left;">
Proprtion mediated: 37 (%)
</td>
</tr>
</tbody>
</table>

### Transfusion (any) - all mediator joint model

Next we examine whether all mediators in a joint model contribute any
additional mediation over and above Hct

``` r
final_df1 <- final_df %>% filter(!is.na(hct_week1_peak)&
                                  !is.na(cumalitive_blood_vol_sampled)&
                                  !is.na(arterial_lines)&
                                  !is.na(mech_vent_combined))
                                  #&
                                  #!is.na(GA_weeks_and_days_integer)&
                                  #!is.na(multiple))


#==========================================================================================================
impData <- medflex::neImpute(rc_transfusion_titans ~ factor(treatment_cat1) +
                     hct_week1_peak +cumalitive_blood_vol_sampled +  arterial_lines+ mech_vent_combined +
                    GA_weeks_and_days_integer +
                    multiple,
                    family = binomial("logit"), nMed = 4, data = final_df1)



neMod.extra.cont <- medflex::neModel(rc_transfusion_titans ~ treatment_cat10+
                              treatment_cat11+
                    GA_weeks_and_days_integer +
                    multiple,
                  family = binomial("logit"),
                  expData = impData,
                  se = "robust")

cont.extra<-data.frame(est = neMod.extra.cont$neModelFit$coefficients, confint(neMod.extra.cont))

lht <- medflex::neLht(neMod.extra.cont, linfct = c("treatment_cat101 = 0", 
                                          "treatment_cat111  = 0", 
                                          "treatment_cat101 + treatment_cat111  = 0"))
t<-summary(lht)

cat.extra<-data.frame(est = exp(t$coefficients[,1]), exp(confint(lht)))

#pte.direct.cat<-(t$coefficients[1,1]/t$coefficients[3,1])*100
pte.indirect.cat<-(t$coefficients[2,1]/t$coefficients[3,1])*100

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
   "95%_CI_U"= `X95..UCL`,)
row.names(cat.extra) <- NULL


joint.M1M2.model_transfusion_any <- cat.extra

joint.M1M2.model_transfusion_any %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Effect
</th>
<th style="text-align:right;">
Estimate
</th>
<th style="text-align:right;">
95%\_CI_L
</th>
<th style="text-align:right;">
95%\_CI_U
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Direct effect
</td>
<td style="text-align:right;">
0.7564983
</td>
<td style="text-align:right;">
0.5748746
</td>
<td style="text-align:right;">
0.9955034
</td>
</tr>
<tr>
<td style="text-align:left;">
Indirect effect
</td>
<td style="text-align:right;">
0.9095878
</td>
<td style="text-align:right;">
0.8054151
</td>
<td style="text-align:right;">
1.0272342
</td>
</tr>
<tr>
<td style="text-align:left;">
Total effect
</td>
<td style="text-align:right;">
0.6881016
</td>
<td style="text-align:right;">
0.5132745
</td>
<td style="text-align:right;">
0.9224768
</td>
</tr>
</tbody>
</table>

``` r
prop_mediated_joint_model_trans_any <-  round((t$coef[3]/(t$coef[2]+t$coef[3])*100),0)

paste0("Proprtion mediated: ", prop_mediated_joint_model_trans_any ," (%)") %>% kable(col.names=NULL)
```

<table>
<tbody>
<tr>
<td style="text-align:left;">
Proprtion mediated: 25 (%)
</td>
</tr>
</tbody>
</table>

# Table of results

``` r
M1_model_results <- bind_rows(list("M1 only: transfusion_any"=M1.model_transfusion_any), .id="id")
M1_model_results <- M1_model_results %>% mutate(across(c(Estimate, `95%_CI_L`, `95%_CI_U`), round,digits=2) )
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
0.76 (0.58-0.99)
</td>
<td style="text-align:left;">
M1M2 joint model: transfusion_any
</td>
<td style="text-align:left;">
Direct effect
</td>
<td style="text-align:left;">
0.76 (0.57-1)
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
0.91 (0.81-1.03)
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
0.65 (0.5-0.85)
</td>
<td style="text-align:left;">
M1M2 joint model: transfusion_any
</td>
<td style="text-align:left;">
Total effect
</td>
<td style="text-align:left;">
0.69 (0.51-0.92)
</td>
</tr>
</tbody>
</table>
