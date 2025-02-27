# purescript-cardano-provider

This library contains a unified interface for cardano provider both for querying but also submitting data on-chain.

The corresponding type is `Provider` defined in the `Cardano.Provider.Type` module.

Additionally, the library defines types specific to a provider:
- various errors that can be propagated throught the Provider defined in `Cardano.Provider.Error` module
- types representing transaction evaluation errors or entities specific to a transaction evaluation, all defined in the `Cardano.Provider.TxEvaluation` module
- `ServerConfig` type
- Affjax `request` helper