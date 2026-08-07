#' Stratifikasi Indeks Konsentrasi
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
#' Tahapan utama fungsi (versi sederhana):
#' \enumerate{
#'   \item Menghitung nilai referensi (Q1, median, mean, atau persentil tertentu) per domain.
#'   \item Menggabungkan nilai referensi tersebut ke data utama.
#'   \item Menghitung indeks konsentrasi (\code{N / ref}) hanya jika \code{N > Ref} dan \code{N > 0}. 
#'         Jika tidak, IK = 0.
#'   \item Melakukan perankingan variabel per baris berdasarkan indeks tertinggi (dengan \code{N} sebagai tie-breaker terkecil).
#'   \item Menentukan \code{strata_final}: ambil \code{r1} jika tidak 0, jika \code{r1 = 0} maka "Non konsentrasi".
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

  # --- 1. Hitung nilai referensi per domain
  fun_ref <- switch(
    ref_method,
    "q1" = function(x) quantile(x[x > 0], 0.25, na.rm = TRUE),
    "median" = function(x) median(x[x > 0], na.rm = TRUE),
    "mean" = function(x) mean(x[x > 0], na.rm = TRUE),
    "percentile" = function(x) quantile(x[x > 0], ref_percentile, na.rm = TRUE)
  )

  ref_domain <- dt[, lapply(.SD, fun_ref), by = domain, .SDcols = varname]
  setnames(ref_domain, varname, paste0(varname, "_ref"))

  # --- 2. Gabungkan nilai referensi ke data utama
  dt <- merge(dt, ref_domain, by = domain, all.x = TRUE)

  # --- 3. Hitung indeks konsentrasi (N / ref) dengan filter N > Ref
  #     Jika N == 0 atau N <= Ref, maka IK = 0
  for (v in varname) {
    dt[[paste0(v, "_ik")]] <- ifelse(
      dt[[v]] > 0 & dt[[v]] > dt[[paste0(v, "_ref")]],  # Syarat: N > 0 DAN N > Ref
      dt[[v]] / dt[[paste0(v, "_ref")]],
      0
    )
  }

  # --- 4. Ranking per baris berdasarkan indeks
  ik_mat <- as.matrix(dt[, paste0(varname, "_ik"), with = FALSE])
  N_mat  <- as.matrix(dt[, varname, with = FALSE])

  # Buat ranking untuk setiap baris
  rank_list <- lapply(seq_len(nrow(ik_mat)), function(i) {
    ik <- ik_mat[i, ]
    N <- N_mat[i, ]
    df <- data.frame(var = varname, ik = ik, N = N)
    
    # Hanya ambil variabel dengan IK > 0 (yang sudah pasti N > Ref)
    df <- df[df$ik > 0, ]
    
    # Jika tidak ada variabel yang memenuhi syarat, return vektor kosong
    if (nrow(df) == 0) return(character(0))
    
    # Urutkan dari IK terbesar, jika sama gunakan N terkecil
    df <- df[order(-df$ik, df$N), ]
    
    # Kembalikan hanya nama variabel
    df$var
  })

  # --- 5. Konversi ke data.table dengan padding dinamis
  # Cari panjang maksimum ranking
  max_rank <- max(sapply(rank_list, length))
  
  if (max_rank == 0) {
    # Kasus ekstrim: tidak ada satupun variabel yang memenuhi syarat di semua baris
    rank_dt <- dt[, c(id, domain), with = FALSE]
    rank_dt[, strata_final := "Non konsentrasi"]
    # Tambahkan kolom r1..rk kosong
    for (j in seq_along(varname)) {
      rank_dt[, paste0("r", j) := NA_character_]
    }
  } else {
    # Padding setiap vektor ranking dengan NA sampai panjang max_rank
    rank_padded <- lapply(rank_list, function(x) {
      if (length(x) < max_rank) {
        c(x, rep(NA_character_, max_rank - length(x)))
      } else {
        x
      }
    })
    
    # Konversi ke matriks
    rank_vars <- do.call(rbind, rank_padded)
    colnames(rank_vars) <- paste0("r", seq_len(max_rank))
    
    # Gabungkan dengan ID dan domain
    rank_dt <- cbind(dt[, c(id, domain), with = FALSE], rank_vars)
    
    # --- 6. Tentukan strata_final (sederhana: ambil r1, jika NA maka Non konsentrasi)
    rank_dt[, strata_final := ifelse(
      is.na(r1) | r1 == "", 
      "Non konsentrasi", 
      r1
    )]
  }

  # --- 7. Kembalikan hasil
  return(list(
    strata = rank_dt,
    ref_values = ref_domain,
    ref_method = ref_method
  ))
}