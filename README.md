# Hiper - Viper insired configuration Haskell library

Viper is a complete configuration solution for go applications including 12 factor apps. It is designed to work within an application, and can handle all types of configuration needs and formats. It supports:

setting defaults
reading from JSON, TOML, YAML, HCL, and Java properties config files
live watching and re-reading of config files (optional)
reading from environment variables
reading from remote config systems (etcd or Consul), and watching changes
reading from command line flags
reading from buffer
setting explicit values
Viper can be thought of as a registry for all of your applications configuration needs.
