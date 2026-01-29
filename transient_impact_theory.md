# Asian Option Pricing with Transient and Permanent Price Impact

## 1. Introduction

This document extends the permanent price impact model for Asian options to incorporate **transient impact**. The original model (Tiwari & Majumdar, 2025) assumes that all price impact from hedging is permanent. In practice, market impact has both permanent and transient components, where transient impact decays over time following an exponential decay pattern.

## 2. The Transient Impact Model

### 2.1 Stock Price Dynamics

Consider a binomial tree with $n$ time periods. At each time step $m \in \{0, 1, ..., n-1\}$, the market maker hedges by trading volume $v_m > 0$. Each move is characterized by:
- $\epsilon_m = +1$ for an up move
- $\epsilon_m = -1$ for a down move

The stock price evolution from time $m$ to time $m+1$ incorporates:

1. **Permanent Impact**: The immediate, persistent effect of hedging volume $v_m$ at time $m$
2. **Transient Impact**: The cumulative, decaying effect of all historical hedging volumes $\{v_0, v_1, ..., v_m\}$

#### Stock Price Dynamics

$$S_{m+1} = \begin{cases}
u \cdot S_m \cdot \exp\left(\lambda_P v_m^\psi + \lambda_T \sum_{k=0}^{m} \alpha^{m-k} \epsilon_k v_k^\psi\right) & \text{if up move at } m \\[10pt]
d \cdot S_m \cdot \exp\left(-\lambda_P v_m^\psi + \lambda_T \sum_{k=0}^{m} \alpha^{m-k} \epsilon_k v_k^\psi\right) & \text{if down move at } m
\end{cases}$$

**Parameters:**
- $u, d$: Base up and down factors in CRR model (with $u > d > 0$)
- $\lambda_P \geq 0$: Permanent impact coefficient
- $\lambda_T \geq 0$: Transient impact coefficient
- $\alpha \in [0,1)$: Decay rate of transient impact ($\alpha = 0$ means no transient impact)
- $\psi > 0$: Power-law exponent for volume (typically $\psi \in [0.5, 1]$ empirically)
- $v_m > 0$: Hedging volume at time $m$ (now **path-dependent**)
- $r > 1$: Gross risk-free rate per period

### 2.2 Path-Dependent Adjusted Factors

Define the **transient impact accumulator** at time $m$ given path history $\omega = (\epsilon_0, \epsilon_1, ..., \epsilon_{m-1})$:

$$I_m(\omega) = \sum_{k=0}^{m-1} \alpha^{m-1-k} \epsilon_k v_k^\psi$$

Note: $I_0 = 0$ (no history at initial time).

The **adjusted up and down factors** at time $m$ depend on the path history:

$$\begin{aligned}
\tilde{u}_m(\omega) &= u \cdot \exp\left(\lambda_P v_m^\psi + \lambda_T \left[I_m(\omega) + v_m^\psi\right]\right) \\[8pt]
\tilde{d}_m(\omega) &= d \cdot \exp\left(-\lambda_P v_m^\psi + \lambda_T \left[I_m(\omega) - v_m^\psi\right]\right)
\end{aligned}$$

**Key Feature**: Unlike the permanent impact model where $\tilde{u}$ and $\tilde{d}$ are constant, here they are **path-dependent** and **time-varying** due to the accumulation of transient impact.

### 2.3 Recursive Update for Transient Impact

The transient impact accumulator updates recursively:

$$I_{m+1}(\omega, \epsilon_m) = \alpha \cdot I_m(\omega) + \epsilon_m v_m^\psi$$

This shows that past impacts decay by factor $\alpha$ at each step, while the current move contributes $\epsilon_m v_m^\psi$.

## 3. Risk-Neutral Valuation

### 3.1 Time and Path-Dependent Risk-Neutral Probability

The **adjusted risk-neutral probability** at time $m$ given path $\omega$ is:

$$p_{m}^{\text{adj}}(\omega) = \frac{r - \tilde{d}_m(\omega)}{\tilde{u}_m(\omega) - \tilde{d}_m(\omega)}$$

Substituting the adjusted factors:

$$p_{m}^{\text{adj}}(\omega) = \frac{r - d \cdot e^{-\lambda_P v_m^\psi + \lambda_T(I_m(\omega) - v_m^\psi)}}{u \cdot e^{\lambda_P v_m^\psi + \lambda_T(I_m(\omega) + v_m^\psi)} - d \cdot e^{-\lambda_P v_m^\psi + \lambda_T(I_m(\omega) - v_m^\psi)}}$$

Factoring out $e^{\lambda_T I_m(\omega)}$:

$$p_{m}^{\text{adj}}(\omega) = \frac{r - d \cdot e^{-\lambda_P v_m^\psi - \lambda_T v_m^\psi + \lambda_T I_m(\omega)}}{u \cdot e^{\lambda_P v_m^\psi + \lambda_T v_m^\psi + \lambda_T I_m(\omega)} - d \cdot e^{-\lambda_P v_m^\psi - \lambda_T v_m^\psi + \lambda_T I_m(\omega)}}$$

Let $\lambda_{\text{eff}} = \lambda_P + \lambda_T$ (effective impact on current period):

$$p_{m}^{\text{adj}}(\omega) = \frac{r - d \cdot e^{-\lambda_{\text{eff}} v_m^\psi} \cdot e^{\lambda_T I_m(\omega)}}{u \cdot e^{\lambda_{\text{eff}} v_m^\psi} \cdot e^{\lambda_T I_m(\omega)} - d \cdot e^{-\lambda_{\text{eff}} v_m^\psi} \cdot e^{\lambda_T I_m(\omega)}}$$

Dividing numerator and denominator by $e^{\lambda_T I_m(\omega)}$:

$$\boxed{p_{m}^{\text{adj}}(\omega) = \frac{r - d \cdot e^{-\lambda_{\text{eff}} v_m^\psi}}{u \cdot e^{\lambda_{\text{eff}} v_m^\psi} - d \cdot e^{-\lambda_{\text{eff}} v_m^\psi}}}$$

**Important Observation**: When volumes are constant ($v_m = v$ for all $m$), the risk-neutral probability simplifies to a constant independent of the path:

$$p^{\text{adj}} = \frac{r - d \cdot e^{-(\lambda_P + \lambda_T) v^\psi}}{u \cdot e^{(\lambda_P + \lambda_T) v^\psi} - d \cdot e^{-(\lambda_P + \lambda_T) v^\psi}}$$

This shows that **for constant volumes, transient impact affects pricing only through the effective total impact** $\lambda_P + \lambda_T$.

### 3.2 No-Arbitrage Conditions

For valid risk-neutral probability, we require $0 \leq p_m^{\text{adj}}(\omega) \leq 1$ for all $m$ and all paths $\omega$.

This translates to:

$$\tilde{d}_m(\omega) < r < \tilde{u}_m(\omega) \quad \forall m, \omega$$

Explicitly:

$$d \cdot \exp\left(-\lambda_P v_m^\psi + \lambda_T(I_m(\omega) - v_m^\psi)\right) < r < u \cdot \exp\left(\lambda_P v_m^\psi + \lambda_T(I_m(\omega) + v_m^\psi)\right)$$

**Minimum Volume Requirements** (assuming worst-case transient accumulation):

Define:
$$I_m^{\max} = \frac{v_{\max}^\psi}{1-\alpha} \quad \text{(maximum possible accumulator)}$$
$$I_m^{\min} = -\frac{v_{\max}^\psi}{1-\alpha} \quad \text{(minimum possible accumulator)}$$

Then volumes must satisfy:

$$v_m^\psi \geq \frac{1}{\lambda_P + \lambda_T} \ln\left(\frac{r}{u}\right) - \frac{\lambda_T}{\lambda_P + \lambda_T} I_m^{\max}$$

$$v_m^\psi \geq -\frac{1}{\lambda_P + \lambda_T} \ln\left(\frac{r}{d}\right) + \frac{\lambda_T}{\lambda_P + \lambda_T} I_m^{\min}$$

## 4. Geometric Asian Option Pricing

### 4.1 Path Representation and Geometric Average

A complete path in the binomial tree is represented by $\omega \in \{+1,-1\}^n$, where $\omega = (\epsilon_0, \epsilon_1, ..., \epsilon_{n-1})$.

The stock price at time $j$ along path $\omega$ is:

$$S_j(\omega) = S_0 \prod_{i=0}^{j-1} \tilde{u}_i^{\mathbb{1}_{\epsilon_i=+1}} \tilde{d}_i^{\mathbb{1}_{\epsilon_i=-1}}$$

where $\tilde{u}_i$ and $\tilde{d}_i$ depend on the path history up to time $i$.

The **geometric average** along path $\omega$ is:

$$G_n(\omega) = \left(\prod_{j=0}^{n} S_j(\omega)\right)^{1/(n+1)}$$

### 4.2 Logarithmic Representation

Taking logarithms:

$$\log G_n(\omega) = \frac{1}{n+1} \sum_{j=0}^{n} \log S_j(\omega)$$

$$= \frac{1}{n+1} \left[(n+1)\log S_0 + \sum_{j=0}^{n} \sum_{i=0}^{j-1} \left(\epsilon_i \log\tilde{u}_i \cdot \mathbb{1}_{\epsilon_i=+1} + (1-\epsilon_i)\log\tilde{d}_i \cdot \mathbb{1}_{\epsilon_i=-1}\right)\right]$$

Let's define cumulative contributions:

$$A(\omega) = \sum_{j=0}^{n} \sum_{i=0}^{j-1} \epsilon_i \left[\lambda_P v_i^\psi + \lambda_T I_i(\omega) + \lambda_T \epsilon_i v_i^\psi\right] \mathbb{1}_{\epsilon_i=+1}$$

$$B(\omega) = \sum_{j=0}^{n} \sum_{i=0}^{j-1} |\epsilon_i| \left[\lambda_P v_i^\psi - \lambda_T I_i(\omega) - \lambda_T \epsilon_i v_i^\psi\right] \mathbb{1}_{\epsilon_i=-1}$$

Then:

$$G_n(\omega) = S_0 \cdot \exp\left(\frac{A(\omega) - B(\omega)}{n+1}\right) \cdot u^{a(\omega)/(n+1)} \cdot d^{b(\omega)/(n+1)}$$

where $a(\omega) = \sum_{j=0}^{n} \#\{\text{ups in } [0,j)\}$ and $b(\omega) = \sum_{j=0}^{n} \#\{\text{downs in } [0,j)\}$.

### 4.3 Option Price Formula

The geometric Asian call option price at time 0 is:

$$\boxed{V_0^G = \frac{1}{r^n} \sum_{\omega \in \{+1,-1\}^n} P(\omega) \cdot \max(G_n(\omega) - K, 0)}$$

where the **path probability** under the adjusted risk-neutral measure is:

$$P(\omega) = \prod_{m=0}^{n-1} \left[p_m^{\text{adj}}(\omega_{\leq m}) \cdot \mathbb{1}_{\epsilon_m=+1} + (1-p_m^{\text{adj}}(\omega_{\leq m})) \cdot \mathbb{1}_{\epsilon_m=-1}\right]$$

Here $\omega_{\leq m}$ denotes the path history $(\epsilon_0, ..., \epsilon_{m-1})$.

### 4.4 Computational Complexity

Since the risk-neutral probability and adjusted factors are path-dependent, the tree **does not recombine**. Therefore:
- Number of paths: $2^n$
- Computational complexity: $O(2^n)$ for exact pricing

For practical computation with $n > 20$, Monte Carlo simulation is recommended.

## 5. Arithmetic Asian Option Bounds

### 5.1 Lower Bound via AM-GM Inequality

The arithmetic mean dominates the geometric mean for positive numbers:

$$A_n(\omega) = \frac{1}{n+1}\sum_{j=0}^{n} S_j(\omega) \geq \left(\prod_{j=0}^{n} S_j(\omega)\right)^{1/(n+1)} = G_n(\omega)$$

Therefore, for call options:

$$\max(A_n(\omega) - K, 0) \geq \max(G_n(\omega) - K, 0)$$

Taking risk-neutral expectations and discounting:

$$\boxed{V_0^A \geq V_0^G}$$

The geometric Asian price serves as a **model-free lower bound** for the arithmetic Asian price.

### 5.2 Upper Bound via Reverse AM-GM Inequality

Following Budimir et al. (2001), for path $\omega$:

$$A_n(\omega) \leq G_n(\omega) \cdot \rho(\omega)$$

where the **path-specific multiplier** is:

$$\rho(\omega) = \exp\left[\frac{1}{4} \cdot \frac{(S_{\max}(\omega) - S_{\min}(\omega))^2}{S_{\min}(\omega) \cdot S_{\max}(\omega)}\right]$$

with:
- $S_{\max}(\omega) = \max_{0 \leq j \leq n} S_j(\omega)$
- $S_{\min}(\omega) = \min_{0 \leq j \leq n} S_j(\omega)$

**Path-specific upper bound:**

$$\max(A_n(\omega) - K, 0) \leq G_n(\omega) \cdot (\rho(\omega) - 1) + \max(G_n(\omega) - K, 0)$$

Taking expectations:

$$\boxed{V_0^A \leq V_0^G + \frac{1}{r^n} \sum_{\omega} P(\omega) \cdot G_n(\omega) \cdot (\rho(\omega) - 1)}$$

### 5.3 Global Upper Bound

To obtain a tractable bound without path enumeration, bound the extremal prices:

$$S_{\max}(\omega) \leq S_0 \cdot u^n \cdot \exp\left[n \lambda_P v_{\max}^\psi + \lambda_T \frac{v_{\max}^\psi}{1-\alpha}\right] =: S_{\max}^*$$

$$S_{\min}(\omega) \geq S_0 \cdot d^n \cdot \exp\left[-n \lambda_P v_{\max}^\psi - \lambda_T \frac{v_{\max}^\psi}{1-\alpha}\right] =: S_{\min}^*$$

Define the **global multiplier**:

$$\rho^* = \exp\left[\frac{1}{4} \cdot \frac{(S_{\max}^* - S_{\min}^*)^2}{S_{\min}^* \cdot S_{\max}^*}\right]$$

Then:

$$\boxed{V_0^A \leq V_0^G + \frac{\rho^* - 1}{r^n} \mathbb{E}^Q[G_n]}$$

**Summary: Two-sided Bounds**

$$\boxed{V_0^G \leq V_0^A \leq V_0^G + \frac{\rho^* - 1}{r^n} \mathbb{E}^Q[G_n]}$$

## 6. Special Cases

### 6.1 Constant Volumes ($v_m = v$ for all $m$)

When volumes are constant:
- Risk-neutral probability becomes path-independent: $p_m^{\text{adj}}(\omega) = p^{\text{adj}}$ for all $m, \omega$
- Effective impact coefficient: $\lambda_{\text{eff}} = \lambda_P + \lambda_T$
- Model reduces to permanent impact model with $\lambda = \lambda_{\text{eff}}$

$$p^{\text{adj}} = \frac{r - d \cdot e^{-\lambda_{\text{eff}} v^\psi}}{u \cdot e^{\lambda_{\text{eff}} v^\psi} - d \cdot e^{-\lambda_{\text{eff}} v^\psi}}$$

### 6.2 No Transient Impact ($\lambda_T = 0$ or $\alpha = 0$)

The model reduces to the original permanent impact model (Tiwari & Majumdar, 2025):

$$\tilde{u}_m = u \cdot e^{\lambda_P v_m^\psi}, \quad \tilde{d}_m = d \cdot e^{-\lambda_P v_m^\psi}$$

$$p_m^{\text{adj}} = \frac{r - d \cdot e^{-\lambda_P v_m^\psi}}{u \cdot e^{\lambda_P v_m^\psi} - d \cdot e^{-\lambda_P v_m^\psi}}$$

### 6.3 Linear Volume Impact ($\psi = 1$)

The standard empirical specification with linear volume:

$$\tilde{u}_m(\omega) = u \cdot \exp\left(\lambda_P v_m + \lambda_T (I_m(\omega) + v_m)\right)$$

$$\tilde{d}_m(\omega) = d \cdot \exp\left(-\lambda_P v_m + \lambda_T (I_m(\omega) - v_m)\right)$$

### 6.4 Square-Root Impact ($\psi = 0.5$)

Common in limit order book models:

$$\tilde{u}_m(\omega) = u \cdot \exp\left(\lambda_P \sqrt{v_m} + \lambda_T (I_m(\omega) + \sqrt{v_m})\right)$$

## 7. Numerical Implementation

### 7.1 Exact Pricing via Tree Enumeration

For moderate $n \leq 20$:

**Algorithm:**
1. Generate all $2^n$ paths $\omega \in \{+1,-1\}^n$
2. For each path $\omega$:
   - Recursively compute $I_m(\omega)$ for $m = 0, 1, ..., n$
   - Compute $\tilde{u}_m(\omega), \tilde{d}_m(\omega)$ for each $m$
   - Compute $p_m^{\text{adj}}(\omega)$ for each $m$
   - Generate price path $\{S_0, S_1(\omega), ..., S_n(\omega)\}$
   - Compute geometric average $G_n(\omega)$
   - Compute path probability $P(\omega) = \prod_{m=0}^{n-1} p_m^{\text{adj}}(\omega_{\leq m})^{\mathbb{1}_{\epsilon_m=+1}} (1-p_m^{\text{adj}}(\omega_{\leq m}))^{\mathbb{1}_{\epsilon_m=-1}}$
3. Price = $\frac{1}{r^n} \sum_{\omega} P(\omega) \cdot \max(G_n(\omega) - K, 0)$

### 7.2 Monte Carlo Simulation

For large $n > 20$:

**Algorithm:**
1. For $i = 1$ to $N_{\text{sim}}$:
   - Initialize $S_0$, $I_0 = 0$
   - For $m = 0$ to $n-1$:
     - Compute $p_m^{\text{adj}}$ given current $I_m$
     - Sample $\epsilon_m \sim \text{Bernoulli}(p_m^{\text{adj}})$
     - Update $S_{m+1}$ based on $\epsilon_m$
     - Update $I_{m+1} = \alpha I_m + \epsilon_m v_m^\psi$
   - Compute $G_n^{(i)}$ from price path
   - Store payoff: $V^{(i)} = \max(G_n^{(i)} - K, 0)$
2. Estimate: $\hat{V}_0^G = \frac{1}{r^n} \cdot \frac{1}{N_{\text{sim}}} \sum_{i=1}^{N_{\text{sim}}} V^{(i)}$
3. Standard error: $\text{SE} = \frac{1}{r^n} \cdot \frac{\sigma_V}{\sqrt{N_{\text{sim}}}}$ where $\sigma_V$ is sample standard deviation

## 8. Comparison with Permanent Impact Model

| Feature | Permanent Impact | Transient + Permanent |
|---------|------------------|----------------------|
| Adjusted factors | Constant across time/paths | Path and time-dependent |
| Risk-neutral probability | Constant (if $v_u, v_d$ constant) | Path-dependent (general) |
| Tree recombination | Yes (geometric level) | No |
| Computational complexity | $O(2^n)$ paths | $O(2^n)$ paths |
| Price sensitivity to $\alpha$ | N/A | High for $\alpha \to 1$ |
| Long-term impact | Permanent | Partially mean-reverting |

**Key Insight:** For constant volumes, transient impact acts as an **additive enhancement** to permanent impact, with effective coefficient $\lambda_{\text{eff}} = \lambda_P + \lambda_T$.

## 9. Empirical Considerations

### 9.1 Parameter Estimation

- **$\lambda_P$**: Estimated from long-term price response to trades
- **$\lambda_T$**: Estimated from short-term (intraday) price reversion
- **$\alpha$**: Decay rate estimated from autocorrelation of price impact
- **$\psi$**: Typically $\psi \in [0.5, 0.6]$ from empirical studies (Bouchaud, 2009)

### 9.2 Typical Values

Literature suggests:
- Market impact decays with half-life of $\tau_{1/2} \approx 1$-$5$ minutes
- For binomial periods $\Delta t$: $\alpha = e^{-\Delta t / \tau}$ where $\tau = \tau_{1/2}/\ln(2)$
- Transient component: $\lambda_T \approx (0.5 - 0.8) \cdot \lambda_{\text{total}}$
- Permanent component: $\lambda_P \approx (0.2 - 0.5) \cdot \lambda_{\text{total}}$

## 10. Summary of Key Results

**Theorem 1 (Geometric Asian Price with Transient Impact)**

Under the transient impact model with parameters $(\lambda_P, \lambda_T, \alpha, \psi)$ and path-dependent volumes $\{v_m\}$, the geometric Asian call option price is:

$$V_0^G = \frac{1}{r^n} \sum_{\omega \in \{+1,-1\}^n} P(\omega) \cdot \max(G_n(\omega) - K, 0)$$

where $P(\omega) = \prod_{m=0}^{n-1} p_m^{\text{adj}}(\omega_{\leq m})^{\mathbb{1}_{\epsilon_m=+1}} (1-p_m^{\text{adj}}(\omega_{\leq m}))^{\mathbb{1}_{\epsilon_m=-1}}$

and the risk-neutral probability is path-dependent via the transient accumulator $I_m(\omega) = \sum_{k=0}^{m-1} \alpha^{m-1-k} \epsilon_k v_k^\psi$.

**Theorem 2 (Bounds for Arithmetic Asian Price)**

The arithmetic Asian call option price satisfies:

$$V_0^G \leq V_0^A \leq V_0^G + \frac{\rho^* - 1}{r^n} \mathbb{E}^Q[G_n]$$

where $\rho^*$ depends on maximum attainable spread including transient effects.

## References

1. Tiwari, P., & Majumdar, S. (2025). Asian option valuation under price impact. *arXiv preprint* arXiv:2512.07154.
2. Bouchaud, J.-P. (2009). Price impact. *arXiv preprint* arXiv:0903.2428.
3. Budimir, I., Dragomir, S. S., & Pečarić, J. (2001). Further reverse results for Jensen's discrete inequality. *Journal of Inequalities in Pure and Applied Mathematics*, 2(1), 5.
4. Kyle, A. S. (1985). Continuous auctions and insider trading. *Econometrica*, 1315-1335.
5. Gatheral, J. (2010). No-dynamic-arbitrage and market impact. *Quantitative Finance*, 10(7), 749-759.
