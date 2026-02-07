# HJB Bid/Ask Diagnosis

## What the code does

In `src/hjb_bellman.cpp`, bid and ask are defined from three value functions:

- **V0** = value with no option (baseline; payoff_sign = 0)
- **Vplus** = value when **long** the option (payoff_sign = +1, receive payoff at maturity)
- **Vminus** = value when **short** the option (payoff_sign = -1, pay payoff at maturity)

Formulas (lines 359–360, 388–389):

- **ask** = V0 − Vminus  (seller’s indifference: price at which MM is willing to sell / go short)
- **bid** = Vplus − V0   (buyer’s indifference: price at which MM is willing to buy / go long)

So:

- **Spread** = ask − bid = 2·V0 − Vplus − Vminus.

## Why you can get bid > ask

- With **no impact** and symmetric setup: Vplus = −Vminus, V0 = 0 ⇒ spread = 0 (bid = ask).
- With **price impact**, long and short choose **different** controls (nu): long pushes price up, short pushes price down. So the two problems are not symmetric and we can get:
  - Vplus + Vminus **>** 2·V0  
  ⇒ spread **<** 0 ⇒ **bid > ask**.

So the formulas are **consistent with the model**: the “buyer indifference price” (Vplus − V0) can exceed the “seller indifference price” (V0 − Vminus) when both sides have manipulation value. The demo even documents this as “manipulation surplus” (negative spread).

## Market convention

In real markets, **bid ≤ ask** by convention (the MM’s bid is what they pay to buy, ask is what they charge to sell). So having **bid > ask** in the output is both:

1. **Economically odd** (would imply the MM can buy high and sell low), and  
2. **Confusing** for users who expect bid ≤ ask.

## Conclusion and fix

- **No sign bug**: The assignments `ask = V0 - Vminus` and `bid = Vplus - V0` match the usual indifference-price definitions.
- **Cause of bid > ask**: The HJB model can produce 2·V0 − Vplus − Vminus < 0 under impact, so the raw indifference prices can cross.
- **Fix**: Enforce **bid ≤ ask** in the reported quotes by treating the two indifference prices as a pair and setting:
  - **bid** = min(buyer_indifference, seller_indifference)
  - **ask** = max(buyer_indifference, seller_indifference)
  and optionally returning the raw buyer/seller prices so users can see when the spread was “crossed” and by how much.

This keeps the model unchanged but makes the reported bid/ask consistent with market convention.
