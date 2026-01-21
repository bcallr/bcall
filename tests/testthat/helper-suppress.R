# Helper functions for tests

# Suppress both messages and cat() output from BCall class
quiet_bcall <- function(...) {
  suppressMessages(bcall(...))
}

quiet_bcall_auto <- function(...) {
  suppressMessages(bcall_auto(...))
}
