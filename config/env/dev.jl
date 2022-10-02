using Genie, Logging

Genie.Configuration.config!(
  server_port                     = 8000,
  server_host                     = "0.0.0.0",
  log_level                       = Logging.Info,
  log_to_file                     = false,
  server_handle_static_files      = true,
  path_build                      = "build",
  format_julia_builds             = true,
  format_html_output              = true,
  watch                           = true
)

ENV["JULIA_REVISE"] = "auto"
