# ==============================
# totp.R - TOTP (RFC 6238) helpers for 2FA
# Custom implementation - no mature R package covers this end-to-end.
# Test any enrollment against your authenticator app before relying on it.
# ==============================

base32_alphabet <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZ234567", "")[[1]]

base32_encode <- function(int_bytes) {
  bits <- unlist(lapply(int_bytes, function(b) as.integer(intToBits(b)[8:1])))
  pad_len <- (5 - (length(bits) %% 5)) %% 5
  bits <- c(bits, rep(0L, pad_len))
  n_groups <- length(bits) / 5
  chars <- character(n_groups)
  weights <- c(16, 8, 4, 2, 1)
  for (i in seq_len(n_groups)) {
    chunk <- bits[((i - 1) * 5 + 1):(i * 5)]
    val <- sum(chunk * weights)
    chars[i] <- base32_alphabet[val + 1]
  }
  paste(chars, collapse = "")
}

base32_decode <- function(str) {
  str <- toupper(gsub("=", "", str))
  chars <- strsplit(str, "")[[1]]
  bits <- unlist(lapply(chars, function(ch) {
    val <- match(ch, base32_alphabet) - 1
    as.integer(intToBits(val)[5:1])
  }))
  n_bytes <- length(bits) %/% 8
  bytes <- raw(n_bytes)
  weights <- c(128, 64, 32, 16, 8, 4, 2, 1)
  if (n_bytes > 0) {
    for (i in seq_len(n_bytes)) {
      chunk <- bits[((i - 1) * 8 + 1):(i * 8)]
      val <- sum(chunk * weights)
      bytes[i] <- as.raw(val)
    }
  }
  bytes
}

generate_totp_secret <- function(n_bytes = 10) {
  int_bytes <- as.integer(openssl::rand_bytes(n_bytes))
  base32_encode(int_bytes)
}

int_to_8byte_be <- function(x) {
  bytes <- raw(8)
  for (i in 8:1) {
    bytes[i] <- as.raw(x %% 256)
    x <- x %/% 256
  }
  bytes
}

get_totp_code <- function(secret_base32, time = as.numeric(Sys.time()), step = 30, digits = 6) {
  key <- base32_decode(secret_base32)
  counter <- floor(time / step)
  msg <- int_to_8byte_be(counter)

  hash_raw <- openssl::sha1(msg, key = key)
  hash <- as.integer(hash_raw)  # 20 bytes, values 0-255

  offset0 <- bitwAnd(hash[20], 0x0F)   # 0-indexed offset, 0..15
  p1 <- hash[offset0 + 1]
  p2 <- hash[offset0 + 2]
  p3 <- hash[offset0 + 3]
  p4 <- hash[offset0 + 4]

  bin_code <- bitwAnd(p1, 0x7F) * (2^24) + p2 * (2^16) + p3 * (2^8) + p4
  code <- bin_code %% (10^digits)
  formatC(code, width = digits, format = "d", flag = "0")
}

verify_totp <- function(secret_base32, code, window = 1, step = 30) {
  now <- as.numeric(Sys.time())
  code <- trimws(as.character(code))
  for (w in -window:window) {
    if (identical(get_totp_code(secret_base32, time = now + (w * step)), code)) return(TRUE)
  }
  FALSE
}

totp_uri <- function(secret_base32, username, issuer = "ProjectCity") {
  sprintf("otpauth://totp/%s:%s?secret=%s&issuer=%s",
          utils::URLencode(issuer, reserved = TRUE),
          utils::URLencode(username, reserved = TRUE),
          secret_base32,
          utils::URLencode(issuer, reserved = TRUE))
}
