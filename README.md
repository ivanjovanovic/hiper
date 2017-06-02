# Hiper - Viper insired configuration Haskell library

## Requirements

### Use case

* Simplest, provide defaults. Ask for parsing and return defaults.

### Defaults

* Client can define config type to be read adding type safety to the config
* Client can define which locations should be checked to parse additional configuration

## Source of configiration config
* Instance of the type should be provided as defaults.
* Instance of the Hiper

## Design

* Hiper monad is a Reader monad as it keeps the default configuration.
* Hiper monad is a Writer monad to write all the configuration before parsing the data
* Hiper monad is IO monad as it needs to read config from files and network.

* Intermediate representation for writing to and reading from in a type safe way.
* Example is deiko-config that parses HOCON. Here other formats should be parsed.



## Viper inspiration
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

* Viper has generic interface to getting the params Get("paramName")

## Resources

* Type safe config - https://hackage.haskell.org/package/deiko-config-0.5.0.0/docs/Data-Config.html
* Configurator - Bryan O'Sullivan https://github.com/bos/configurator
