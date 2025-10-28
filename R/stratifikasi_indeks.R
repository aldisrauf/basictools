#' Stratifikasi Indeks Konsentrasi (Versi Cepat)
#'
#' Fungsi ini menghitung indeks konsentrasi dan menentukan strata akhir secara efisien
#' menggunakan paket `data.table`. Proses dilakukan per domain (misalnya per kabupaten/kota),
#' dengan pembandingan nilai setiap variabel terhadap nilai referensi (Q1, median, mean, atau persentil).
#'
#' @param data Data frame yang berisi data mentah.
#' @param id Karakter; nama kolom unik untuk setiap unit (misalnya `bs`).
#' @param domain Karakter; nama kolom domain atau wilayah pengelompokan (misalnya `idkabkot`).
#' @param varname Vektor karakter; nama variabel numerik yang akan dihitung indeksnya.
#' @param ref_method Karakter; metode perhitungan nilai pembanding per domain.
#'   Pilihan: `"q1"`, `"median"`, `"mean"`, atau `"percentile"`. Default `"q1"`.
#' @param ref_percentile Numerik antara 0 dan 1; digunakan bila `ref_method = "percentile"`. Default `0.25`.
#'
#' @return Sebuah list yang berisi tiga komponen:
#' \itemize{
#'   \item \code{strata}: \code{data.table} berisi kolom ID, domain, ranking tiap variabel (`r1`, `r2`, ...), dan \code{strata_final}.
#'   \item \code{ref_values}: Nilai referensi per domain untuk tiap variabel.
#'   \item \code{ref_method}: Metode referensi yang digunakan.
#' }
#'
#' @details
#' Tahapan utama fungsi:
#' \enumerate{
#'   \item Menghitung nilai referensi (Q1, median, mean, atau persentil tertentu) per domain.
#'   \item Menggabungkan nilai referensi tersebut ke data utama.
#'   \item Menghitung indeks konsentrasi (\code{N / ref}) untuk setiap variabel.
#'   \item Melakukan perankingan variabel per baris berdasarkan indeks tertinggi (dengan \code{N} sebagai tie-breaker terkecil).
#'   \item Menentukan \code{strata_final} pertama di mana nilai aktual lebih besar dari nilai referensi.
#' }
#'
#' @examples
#' library(data.table)
#'
#' # Contoh data sederhana
#' df <- data.frame(
#'   idkabkot = c("1101", "1101", "1102", "1102"),
#'   bs = c("BS01", "BS02", "BS03", "BS04"),
#'   JAGUNG = c(10, 0, 5, 15),
#'   KEDELAI = c(8, 2, 0, 6),
#'   UBIKAYU = c(20, 5, 0, 10)
#' )
#'
#' result <- stratifikasi_indeks(
#'   data = df,
#'   id = "bs",
#'   domain = "idkabkot",
#'   varname = c("JAGUNG", "KEDELAI", "UBIKAYU"),
#'   ref_method = "q1"
#' )
#'
#' # Lihat hasil strata akhir
#' head(result$strata)
#'
#' # Nilai referensi per domain
#' result$ref_values
#'
#' @export
#' @import data.table
stratifikasi_indeks <- function(data, id, domain, varname,
                                ref_method = "q1",
                                ref_percentile = 0.25) {

  ref_method <- tolower(ref_method)
  if (!ref_method %in% c("q1", "median", "mean", "percentile"))
    stop('ref_method harus salah satu dari: "q1", "median", "mean", atau "percentile"')

  dt <- as.data.table(data)

  # --- Hitung nilai referensi per domain
  fun_ref <- switch(
    ref_method,
    "q1" = function(x) quantile(x[x > 0], 0.25, na.rm = TRUE),
    "median" = function(x) median(x[x > 0], na.rm = TRUE),
    "mean" = function(x) mean(x[x > 0], na.rm = TRUE),
    "percentile" = function(x) quantile(x[x > 0], ref_percentile, na.rm = TRUE)
  )

  ref_domain <- dt[, lapply(.SD, fun_ref), by = domain, .SDcols = varname]
  setnames(ref_domain, varname, paste0(varname, "_ref"))

  # --- Gabungkan nilai referensi ke data utama
  dt <- merge(dt, ref_domain, by = domain, all.x = TRUE)

  # --- Hitung indeks konsentrasi (N / ref)
  for (v in varname) {
    dt[[paste0(v, "_ik")]] <- ifelse(
      dt[[v]] > 0,
      dt[[v]] / dt[[paste0(v, "_ref")]],
      0
    )
  }

  # --- Ranking per baris berdasarkan indeks
  ik_mat <- as.matrix(dt[, paste0(varname, "_ik"), with = FALSE])
  N_mat  <- as.matrix(dt[, varname, with = FALSE])

  rank_list <- lapply(seq_len(nrow(ik_mat)), function(i) {
    ik <- ik_mat[i, ]
    N <- N_mat[i, ]
    df <- data.frame(var = varname, ik = ik, N = N)
    df <- df[df$ik > 0, ]
    if (nrow(df) == 0) return(rep(0, length(varname)))

    # Urutkan dari IK terbesar, jika sama gunakan N terkecil
    df <- df[order(-df$ik, df$N), ]
    c(df$var, rep(0, length(varname) - nrow(df)))
  })

  rank_vars <- do.call(rbind, rank_list)
  colnames(rank_vars) <- paste0("r", seq_along(varname))
  rank_dt <- cbind(dt[, c(id, domain), with = FALSE], rank_vars)

  # --- Tentukan strata_final
  strata_final <- rep("Non konsentrasi", nrow(rank_dt))

  for (j in seq_along(varname)) {
    rv <- rank_dt[[paste0("r", j)]]
    idx <- which(strata_final == "Non konsentrasi" & rv != 0)
    if (length(idx) > 0) {
      Nval <- mapply(function(i, v) dt[[v]][i], idx, rv[idx])
      Refval <- mapply(function(i, v) dt[[paste0(v, "_ref")]][i], idx, rv[idx])
      strata_final[idx[Nval > Refval]] <- rv[Nval > Refval]
    }
  }

  rank_dt[, strata_final := strata_final]

  return(list(
    strata = rank_dt,
    ref_values = ref_domain,
    ref_method = ref_method
  ))
}
