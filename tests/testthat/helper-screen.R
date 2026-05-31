# Test helpers for planscanR.screen.

# Deterministic, language-agnostic mock embedding model used across the
# relevance/topics tests. We never touch reticulate / sentence-transformers in
# the automated suite — those would require a live Python environment.
make_fake_model <- function(languages = c("nl", "en", "de"), dim = 64L) {
  embedding_model(
    name = "fake-bow",
    languages = languages,
    embed_fn = function(x) {
      # Tiny hashing-vectorizer bag-of-words: tokenize on word boundaries,
      # hash each token to one of `dim` slots, count occurrences. Real token
      # overlap drives cosine similarity.
      slot_of <- function(tok) (sum(utf8ToInt(tok)) %% dim) + 1L
      do.call(
        rbind,
        lapply(x, function(s) {
          toks <- tolower(strsplit(s, "\\W+")[[1]])
          toks <- toks[nzchar(toks)]
          v <- numeric(dim)
          for (t in toks) {
            v[slot_of(t)] <- v[slot_of(t)] + 1
          }
          v
        })
      )
    }
  )
}
