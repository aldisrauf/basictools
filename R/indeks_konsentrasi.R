#' Concentration Index Calculation
#'
#' The `indeks_konsentrasi` function calculates concentration indices, generates rankings, and evaluates strata within specified domains. It allows flexible customization of analysis based on specified variables and domains.
#'
#' @param data A data frame. The dataset containing variables for concentration analysis.
#' @param variables Character vector. Names of the variables to analyze for concentration indices.
#' @param domain Character. The column name specifying domain grouping (e.g., provinces or regions).
#' @param rank_by Character. Method for ranking strata. Options are `"mean"` for ranking by mean values and `"q1"` for ranking by the first quartile.
#' @param eval_type Integer. The type of evaluation to perform:
#'   \itemize{
#'     \item \code{1}: Aggregate evaluation at the domain level.
#'     \item \code{2}: Evaluation of top-ranked strata within domains.
#'     \item \code{3}: Hierarchical evaluation for specific levels.
#'     \item \code{4}: Combined stratified evaluation across multiple domains.
#'   }
#' @param threshold Numeric. A percentile threshold for concentration evaluation. Default is \code{0}.
#' @param include_zeros Logical. Whether to include zero values in concentration calculations. Default is \code{TRUE}.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{processed_data}: Data frame with computed concentration indices and ranks.
#'     \item \code{aggregated_summary}: Summary table of evaluated results across domains.
#'     \item \code{inactive_data}: Rows flagged as inactive based on evaluation criteria.
#'   }
#'
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   strata = c("A", "B", "C", "D"),
#'   value = c(10, 20, 0, 15),
#'   domain = c("X", "X", "Y", "Y")
#' )
#'
#' # Function call
#' result <- indeks_konsentrasi(
#'   data = data,
#'   variables = "value",
#'   domain = "domain",
#'   rank_by = "mean",
#'   eval_type = 1,
#'   threshold = 0.1,
#'   include_zeros = TRUE
#' )
#'
#' # Output results
#' print(result$processed_data)
#' print(result$aggregated_summary)
#'
#' @export
indeks_konsentrasi <- function(data, variables, domain, rank_by, eval_type, threshold = 0, include_zeros = TRUE) {
  indeks_konsentrasi = function(
    id, varname, domain, data, eval = "mean", percentile = NULL, eval_type = c(1,2,3,4), rank_type = c("mean", "q1"), remove_zero_eval = F
  ){
    if(!(eval %in% c("mean", "percentile"))){stop("eval hanya bernilai mean atau percentile")}
    if(!(eval_type %in% c(1:4))){stop("eval_type hanya bernilai 1, 2, 3, 4")}
    if(!(rank_type %in% c("mean", "q1"))){stop("rank_type hanya bernilai mean atau q1")}
    if(eval == "percentile" & is.null(percentile)){stop("jika eval = percentile, maka percentile harus terisi 0 s.d. 1")}
    if(eval == "percentile"){
      if(percentile<=0 & percentile>=1){stop("persentile hanya bernilai antara 0 s.d. 1")}
    }
    #Output
    output_final = list()

    # Prepare Data
    dt = data %>% select(c(domain, id, varname))

    # Replace NA jadi 0
    dt = dt %>%
      mutate(across(varname, ~replace_na(.x, 0)))

    # Filter non aktif
    dt = dt %>%
      mutate(total = rowSums(dt[-c(1:(length(id) + length(domain)))])) %>%
      filter(total>0) %>%
      select(-total)

    dt_nonaktif = dt %>%
      mutate(total = rowSums(dt[-c(1:(length(id) + length(domain)))])) %>%
      filter(total==0) %>%
      select(-total)

    # Hitung rata-rata
    temp1 = dt %>%
      select(paste0(varname))



    if(rank_type == "mean"){
      function_rank = function(x){mean(x[x>0], na.rm=T)}
    }else{
      if(rank_type == "q1"){
        function_rank = function(x){quantile(x[x>0], 0.25, na.rm=T)}
      }
    }

    temp2 = dt %>%
      group_by_at(domain) %>%
      mutate(across(varname, list(mean = function_rank))) %>%
      ungroup() %>%
      select(paste0(varname, "_mean"))

    # Hitung indeks konsentrasi
    temp3 = data.frame(temp1/temp2) %>% set_names(paste0(varname, "_ik"))

    # Hitung peringkat
    temp4 = data.frame(t(apply(-temp3, 1, rank, ties.method = "max"))) %>% set_names(paste0(varname, "_rank"))

    a=""
    for (i in 1:ncol(temp3)) {
      if(i==1){
        a =paste0(paste0(paste0(varname[i],"_rank2 = "),"ifelse(", colnames(temp3)[i],"==0, ", 0,", ", colnames(temp4)[i], ")"))
      }else{
        a =paste(a, paste0(paste0(varname[i],"_rank2 = "),"ifelse(", colnames(temp3)[i],"==0, ", 0,", ", colnames(temp4)[i], ")"), sep = ", ")
      }
    }
    rank2 = paste0("temp5 = data.frame(cbind(temp3, temp4)) %>% mutate(", a, ")")

    eval(parse(text = rank2))

    temp5 = temp5 %>% select(paste0(varname, "_rank2")) %>% set_names(paste0(varname))

    temp6 = dt %>% select(id) %>%
      cbind(temp5)

    temp7_ = pivot_longer(
      data = temp6,
      cols = !id,
      names_to = "strata",
      values_to = "rank"
    ) %>% group_by_at(id) %>%
      arrange_at(c(id, "rank")) %>%
      mutate(rank = glue::glue("top{rank}") %>% as.character()) %>%
      select_at(c(id, "rank", "strata")) %>%
      filter(rank != "top0")

    temp7 = temp7_ %>%
      left_join(
        temp7_ %>% filter(rank == "top1") %>% group_by(strata) %>% tally(), "strata"
      ) %>%
      arrange_at(c(id, "rank", "n")) %>%
      group_by(idsls) %>%
      mutate(
        rank = paste0("top", 1:n())
      ) %>% select(-n) %>%
      pivot_wider(names_from = "rank", values_from = "strata")

    rankname = colnames(temp7)[-1]

    rankname = paste0(
      "temp8 = temp7 %>% mutate(",
      paste(paste0(rankname," = paste0(", rankname,", collapse='_')"), collapse = ", ")
      ,")"
    )

    eval(parse(text=rankname))

    # Evaluasi

    temp8_1 = dt %>%
      left_join(temp8 %>% filter() %>% select(id, strata_awal=top1) %>% ungroup(), by=id)

    i=1
    for (i in 1:length(varname)) {
      tempstrat = temp8_1 %>% filter(strata_awal==varname[i]) %>% select(domain, id, "strata_awal", varname[i])
      if(i==1){
        outmean=tempstrat
      }else{
        outmean=full_join(outmean, tempstrat, by = c(domain, id, "strata_awal"))
      }
    }


    if(eval == "percentile"){
      if(remove_zero_eval){
        function_eval = function(x){quantile(x[x>0], percentile, na.rm=T)}
      }else{
        function_eval = function(x){quantile(x, percentile, na.rm=T)}
      }
    }

    if(eval == "mean"){
      if(remove_zero_eval){
        function_eval = function(x){mean(x[x>0], na.rm=T)}
      }else{
        function_eval = function(x){mean(x, na.rm=T)}
      }
    }

    temp9 =  dt %>% left_join(temp8 %>% select(id, top1, top2) %>% ungroup(), by=id)

    if(eval_type == 1){
      temp8_2 = outmean %>% #replace(is.na(.),0) %>%
        group_by_at(c(domain, "strata_awal")) %>%
        summarise(across(varname, function_eval, .names = "{col}_mean"), .groups = 'drop') %>%
        ungroup() %>%
        group_by_at(domain) %>%
        summarise_if(is.numeric, sum, na.rm=T) %>%
        select(domain, paste0(varname, "_mean"))

      temp9 = dt %>% left_join(temp8 %>% select(id, top1, top2) %>% ungroup(), by=id) %>% left_join(temp8_2, by=domain)
    }

    if(eval_type == 2){
      temp8_2 = temp9 %>%
        group_by(idkab, top1, top2) %>%
        summarise(
          across(varname, function(x){function_eval(x)})
        ) %>%
        mutate(
          across(varname, function(x){ifelse(is.nan(x), 0, x)}, .names = "{col}_mean")
        ) %>%
        select_at(c(domain, "top1", "top2", paste0(varname, "_mean")))
      temp9 = dt %>% left_join(temp8 %>% select(id, top1, top2) %>% ungroup(), by=id) %>% left_join(temp8_2, by=c(domain, "top1", "top2"))
    }

    if(eval_type == 3){
      temp8_2 = temp9 %>%
        group_by(idkab) %>%
        summarise(
          across(varname, function(x){function_eval(x)}, .names = "{col}_mean")
        )
      temp9 = dt %>% left_join(temp8 %>% select(id, top1, top2) %>% ungroup(), by=id) %>% left_join(temp8_2, by=domain)
    }

    if(eval_type == 4){
      temp8_2 = temp9 %>% select_at(c(domain, varname, "top1")) %>%
        group_by_at(c(domain, "top1")) %>%
        summarise(across(varname, mean)) %>%
        pivot_longer(!c(!!sym(domain), top1)) %>%
        filter(top1 == name) %>%
        ungroup() %>%
        select_at(c(domain, "top1", "value")) %>%
        mutate(top1 = paste0(top1, "_mean1")) %>%
        pivot_wider(names_from = top1, values_from = value) %>%
        left_join(
          temp9 %>% select_at(c(domain, varname, "top2")) %>%
            group_by_at(c(domain, "top2")) %>%
            summarise(across(varname, mean)) %>%
            pivot_longer(!c(!!sym(domain), top2)) %>%
            filter(top2 == name) %>%
            ungroup() %>%
            select_at(c(domain, "top2", "value")) %>%
            mutate(top2 = paste0(top2, "_mean2")) %>%
            pivot_wider(names_from = top2, values_from = value), by = domain
        )

      temp9 = dt %>% left_join(temp8 %>% select(id, top1, top2) %>% ungroup(), by=id) %>% left_join(temp8_2, by=domain)
    }

    temp10 = temp9 %>% mutate(
      top1 = ifelse(top1 %in% varname, top1, "NA"),
      top2 = ifelse(top2 %in% varname, top2, "NA")
    )

    # for (i in 1:length(varname)) {
    #   if(i==1){
    #     teks = paste0("strata_eval = ifelse(top2=='NA', ifelse(top1== '",varname[i],"', ifelse(", varname[i],"<", paste0(varname[i], '_mean'), ", 'NK','", varname[i],"'), NA), NA)")
    #     a=teks
    #   }else{
    #     teks = paste0("strata_eval = ifelse(top2=='NA', ifelse(top1== '",varname[i],"', ifelse(", varname[i],"<", paste0(varname[i], '_mean'), ", 'NK','", varname[i],"'), strata_eval), NA)")
    #     a=paste(a, teks, sep=", ")
    #   }
    #   #print(teks)
    # }
    #
    # teks = paste0(
    #   "temp10 = temp9 %>% mutate(", a, ")"
    # )

    #eval(parse(text=teks))

    if(eval_type %in% c(1,2,3)){
      out = temp10 %>%
        rowwise() %>%
        mutate(
          strata_eval = top1,
          val1 = get(paste0(top1)),
          val2 = ifelse(top2 == "NA", 0, get(paste0(top2))),
          eval1 = get(paste0(top1, "_mean")),
          eval2 = ifelse(top2 == "NA", Inf, get(paste0(top2, "_mean")))
        ) %>%
        mutate(
          strata_eval=ifelse(top2!="NA", ifelse(val1>=eval1, top1, ifelse(val2>=eval2, top2,"NK")), ifelse(val1>=eval1, top1, "NK"))
        ) %>% ungroup()
    }else{
      out = temp10 %>%
        rowwise() %>%
        mutate(
          strata_eval = top1,
          val1 = get(paste0(top1)),
          val2 = ifelse(top2 == "NA", 0, get(paste0(top2))),
          eval1 = get(paste0(top1, "_mean1")),
          eval2 = ifelse(top2 == "NA", Inf, get(paste0(top2, "_mean2")))
        ) %>%
        mutate(
          strata_eval=ifelse(top2!="NA", ifelse(val1>=eval1, top1, ifelse(val2>=eval2, top2,"NK")), ifelse(val1>=eval1, top1, "NK"))
        ) %>% ungroup()
    }


    # i=1
    # for (i in 1:nrow(temp10)) {
    #   rowtemp = temp10[i,]
    #   nametop1 = rowtemp %>% pull(top1)
    #   nametop2 = rowtemp %>% pull(top2)
    #   rowtemp = rowtemp %>%
    #     mutate(
    #       strata_eval=ifelse(
    #         top2!="NA", ifelse(
    #           !!as.symbol(nametop1)>=!!as.symbol(paste0(nametop1,"_mean")), nametop1,
    #           ifelse(
    #             !!as.symbol(nametop2)>=!!as.symbol(paste0(nametop2,"_mean")), nametop2,
    #             "NK"
    #           )
    #         ), strata_eval
    #       )
    #     )
    #   if(i==1){
    #     out=rowtemp
    #   }else{
    #     out=rbind(out, rowtemp)
    #   }
    # }

    output_final$strata_final = out
    output_final$summarise = out %>% group_by_at(c(domain, "strata_eval")) %>% summarise(n = n(),.groups = 'drop')
    output_final$nonaktif = dt_nonaktif
    output_final$data = dt

    return(output_final)
  }

}
