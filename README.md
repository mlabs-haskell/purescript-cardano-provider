# purescript-cardano-provider

This library defines a unified `Provider` interface encapsulating various
queries for interacting with the Cardano blockchain, serving as an abstraction
for [cardano-transaction-lib (CTL)](https://github.com/Plutonomicon/cardano-transaction-lib)
query layer components.

The corresponding `Provider` type is defined in the `Cardano.Provider.Type` module.

The library also provides various utility types and other common types for reuse
by `Provider` implementations, including:
  - Errors, defined in the `Cardano.Provider.Error` module.
  - Reusable Ogmios types, defined in the `Cardano.Provider.OgmiosTypes` module.
  - The `ServerConfig` type.
  - The Affjax `request` helper.
