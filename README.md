# Nexus Pay

A type-safe payment gateway library written in Haskell, supporting UPI, card,
and digital wallet (PhonePe, Paytm, AmazonPay) payment methods.

The core design uses a **GADT-based state machine** to enforce payment lifecycle
rules at compile time — invalid state transitions (e.g. capturing an un-authorized
payment) are type errors, not runtime failures.

---

## Architecture

Gateway              ← facade: single import for consumers
├── Gateway.Types        ← core data types (PaymentMethod, Amount, GatewayError…)
├── Gateway.StateMachine ← GADT phantom-typed Payment state machine
├── Gateway.Class        ← PaymentGateway type class (UPI / Card / Wallet dispatch)
├── Gateway.UPI          ← UPI implementation (e.g. priya@phonepe)
├── Gateway.Card         ← Card implementation (masked last-4 display)
├── Gateway.Wallet       ← Wallet implementation (PhonePe, Paytm, AmazonPay)
└── Gateway.Mock         ← deterministic mock for testing



## Payment Lifecycle

Each payment moves through five states — enforced by the type system:

Initiated → Authorized → Captured
↘ Failed
→ Refunded



A function that expects `Payment 'Authorized` will **not compile** if passed a
`Payment 'Initiated` — no runtime guard needed.

## Payment Methods

| Method | Example |
|--------|---------|
| UPI    | `priya@phonepe` |
| Card   | Visa/Mastercard, masked `**** **** **** 4242` |
| Wallet | PhonePe · Paytm · AmazonPay |

## Error Handling

11 typed `GatewayError` variants including `InsufficientFunds`,
`InvalidCredentials`, `NetworkTimeout`, `AmountLimitExceeded`, and more —
all surfaced through `Either GatewayError` return types.

## Running the Demo

```bash
cabal run payment-gateway-demo
Runs 3 successful payment flows (UPI, Card, Wallet) and 3 error scenarios
(invalid UPI, amount limit, method mismatch).

Running Tests

cabal test
Property-based tests via QuickCheck and unit tests via hspec covering
state machine transitions and error paths.

Tech Stack
Haskell (GHC 2010)
GADTs + phantom types for compile-time state enforcement
Type classes for zero-branching provider dispatch
QuickCheck — property-based testing
hspec — BDD-style test runner


---


