use std::f64::consts::E;

use peroxide::fuga::{
    anyhow::Result as PeroxideResult, inv_erf, BisectionMethod, Jaco, Pt, RootFinder,
    RootFindingProblem,
};

/// Computes the factorial of a positive integer
fn factorial(n: u64) -> u64 {
    (1..=n).product()
}

/// Computes the value of the inverse CDF of the standard normal
/// distribution at the desired quantile.
fn normal_inverse_cdf(q: f64) -> f64 {
    if q <= 0.0 || q >= 1.0 {
        panic!("Inverse cdf of the normal distribution can only be computed if the desired quantile q is 0.0 < q < 1.0");
    } else {
        (2.0_f64.sqrt()) * inv_erf(2.0 * q - 1.0)
    }
}

/// Computes the PDF of a random variable X where X ~ Erlang(lambda,
/// k)
fn pdf_erlang(t: Pt<1>, lambda: f64, k: u64) -> PeroxideResult<Jaco<1, 1>> {
    // Extract input value
    let t = t[0];

    // Bounds checking
    if t < 0.0 {
        panic!("Input to erlang PDF was less than 0.0");
    }
    if lambda <= 0.0 {
        panic!("Erlang lambda value was <= 0.0");
    }
    let k_i32: i32 = k
        .try_into()
        .expect("Erlang k value could not be converted to an i32");

    // Evaluate
    let result =
        lambda.powi(k_i32) * t.powi(k_i32 - 1) * E.powf(-lambda * t) / (factorial(k) as f64);
    Ok([[result]])
}

/// Computes the PDF of a random variable Z = X + Y where X ~
/// Exp(exp_lambda), Y ~ Erlang(erlang_lambda, erlang_k)
fn pdf_exp_and_erlang(
    t: Pt<1>,
    exp_lambda: f64,
    erlang_lambda: f64,
    erlang_k: u64,
) -> PeroxideResult<Jaco<1, 1>> {
    // Extract input value
    let t = t[0];

    // Bounds checking
    if t < 0.0 {
        panic!("Input to exp and erlang PDF was less than 0.0");
    }
    if exp_lambda <= 0.0 {
        panic!("Exp lambda value was <= 0.0");
    }
    if erlang_lambda <= 0.0 {
        panic!("Erlang lambda value was <= 0.0");
    }
    let erlang_k_i32: i32 = erlang_k
        .try_into()
        .expect("Erlang k value could not be converted to an i32");

    // If rates are nearly equal, interpret as a single
    // Erlang-distributed variable to avoid issues with floats
    // mis-representing such small differences
    if (erlang_lambda - exp_lambda).abs() < 1e-6 * erlang_lambda.max(exp_lambda) {
        // Get average of the two nearly identical lambda
        // parameters
        let lambda = 0.5 * (exp_lambda + erlang_lambda);

        // Evaluate pdf of the Erlang
        Ok([[
            (lambda.powi(erlang_k_i32 + 1) * t.powi(erlang_k_i32) * (-lambda * t).exp())
                / (factorial(erlang_k) as f64),
        ]])
    } else {
        // Shorthand for difference between erlang and exp
        // parameters
        let a = erlang_lambda - exp_lambda;

        // Computed sum
        let mut s = 0.0;

        // Product of difference in parameters and t is used in
        // multiple places
        let at = a * t;

        // Current value of the term in the summation, (at)^j / j!
        // where j is the summation iterator; when j = 0, term = 1
        let mut term = 1.0;
        for j in 0..erlang_k {
            if j > 0 {
                term *= at / (j as f64);
            }
            s += term;
        }

        // Final computation
        let pref = exp_lambda * (-exp_lambda * t).exp() * (erlang_lambda / a).powi(erlang_k_i32);
        Ok([[pref * (1.0 - (-at).exp() * s)]])
    }
}

/// Computes a PDF where mix_proportion of the PDF is Exp+Erlang
/// distributed and (1.0 - mix_proportion) of the PDF is Erlang
/// distributed
fn pdf_mix(
    t: Pt<1>,
    exp_lambda: f64,
    erlang_lambda: f64,
    erlang_k: u64,
    mix_proportion: f64,
) -> PeroxideResult<Jaco<1, 1>> {
    // Bounds checking
    if !(0.0..=1.0).contains(&mix_proportion) {
        panic!("Proportion of mixed PDF was not between [0.0, 1.0]");
    }

    // Evaluate
    Ok([[
        mix_proportion * pdf_exp_and_erlang(t, exp_lambda, erlang_lambda, erlang_k)?[0][0]
            + (1.0 - mix_proportion) * pdf_erlang(t, erlang_lambda, erlang_k)?[0][0],
    ]])
}

fn cdf_erlang(t: Pt<1>, erlang_lambda: f64, erlang_k: u64) -> PeroxideResult<Pt<1>> {
    // Extract input value
    let t = t[0];

    // Bounds checking
    if t < 0.0 {
        panic!("Input to erlang CDF was less than 0.0");
    }
    if erlang_lambda <= 0.0 {
        panic!("Erlang lambda value was <= 0.0");
    }
    let erlang_k_i32: i32 = erlang_k
        .try_into()
        .expect("Erlang k value could not be converted to an i32");

    // Evaluate
    let mut sum = 0.0;
    for (n_u64, n_i32) in (0..erlang_k).zip(0..erlang_k_i32) {
        sum += (1.0 / factorial(n_u64) as f64) * (erlang_lambda * t).powi(n_i32);
    }
    let total = 1.0 - E.powf(-erlang_lambda * t) * sum;
    Ok([total])
}

fn cdf_exp_and_erlang(
    t: Pt<1>,
    exp_lambda: f64,
    erlang_lambda: f64,
    erlang_k: u64,
) -> PeroxideResult<Pt<1>> {
    // Extract input value
    let t = t[0];

    // Bounds checking
    if t < 0.0 {
        panic!("Input to exp and erlang CDF was less than 0.0");
    }
    if exp_lambda <= 0.0 {
        panic!("Exp lambda value was <= 0.0");
    }
    if erlang_lambda <= 0.0 {
        panic!("Erlang lambda value was <= 0.0");
    }
    let erlang_k_i32: i32 = erlang_k
        .try_into()
        .expect("Erlang k value could not be converted to an i32");

    // Evaluate
    if t == 0.0 {
        Ok([0.0])
    } else if (erlang_lambda - exp_lambda).abs() < 1e-6 * erlang_lambda.max(exp_lambda) {
        let lambda = 0.5 * (exp_lambda + erlang_lambda);
        let mut s = 0.0;
        let lt = lambda * t;
        let mut term = 1.0;
        for m in 0..=erlang_k {
            if m > 0 {
                term *= lt / (m as f64);
            }
            s += term;
        }
        Ok([1.0 - (-lambda * t).exp() * s])
    } else {
        let a = erlang_lambda - exp_lambda;
        let rate_ratio = (erlang_lambda / a).powi(erlang_k_i32);
        let mut sum = 0.0;
        let mut poly = 1.0;
        for m in 0..erlang_k_i32 {
            if m > 0 {
                poly *= (erlang_lambda * t) / (m as f64);
            }
            let coeff = rate_ratio / (erlang_lambda / a).powi(m) - 1.0;
            sum += poly * coeff;
        }
        Ok([1.0 - rate_ratio * (-exp_lambda * t).exp() + (-erlang_lambda * t).exp() * sum])
    }
}

fn cdf_mix(
    t: Pt<1>,
    exp_lambda: f64,
    erlang_lambda: f64,
    erlang_k: u64,
    mix_proportion: f64,
) -> PeroxideResult<Pt<1>> {
    // Bounds checking
    if !(0.0..=1.0).contains(&mix_proportion) {
        panic!("Proportion of mixed CDF was not between [0.0, 1.0]");
    }

    // Evaluate
    Ok([
        mix_proportion * cdf_exp_and_erlang(t, exp_lambda, erlang_lambda, erlang_k)?[0]
            + (1.0 - mix_proportion) * cdf_erlang(t, erlang_lambda, erlang_k)?[0],
    ])
}

fn cdf_residual_erlang(
    t: Pt<1>,
    erlang_lambda: f64,
    erlang_k: u64,
    quantile: f64,
) -> PeroxideResult<Pt<1>> {
    // Bounds checking
    if quantile <= 0.0 || quantile >= 1.0 {
        panic!("Quantile requested was not between (0.0, 1.0)")
    }

    // Evaluate
    let cdf = cdf_erlang(t, erlang_lambda, erlang_k)?[0];
    Ok([cdf - quantile])
}

fn cdf_residual_exp_and_erlang(
    t: Pt<1>,
    exp_lambda: f64,
    erlang_lambda: f64,
    erlang_k: u64,
    quantile: f64,
) -> PeroxideResult<Pt<1>> {
    // Bounds checking
    if quantile <= 0.0 || quantile >= 1.0 {
        panic!("Quantile requested was not between (0.0, 1.0)")
    }

    // Evaluate
    let cdf = cdf_exp_and_erlang(t, exp_lambda, erlang_lambda, erlang_k)?[0];
    Ok([cdf - quantile])
}

fn cdf_residual_mix(
    t: Pt<1>,
    exp_lambda: f64,
    erlang_lambda: f64,
    erlang_k: u64,
    quantile: f64,
    mix_proportion: f64,
) -> PeroxideResult<Pt<1>> {
    // Bounds checking
    if quantile <= 0.0 || quantile >= 1.0 {
        panic!("Quantile requested was not between (0.0, 1.0)")
    }

    // Evaluate
    let cdf = cdf_mix(t, exp_lambda, erlang_lambda, erlang_k, mix_proportion)?[0];
    Ok([cdf - quantile])
}

struct Mix {
    exp_lambda: f64,
    erlang_lambda: f64,
    erlang_k: u64,
    quantile: f64,
    mix_proportion: f64,
}

/// Implementation with two floats as the initial guess for use by the
/// BisectMethod
impl RootFindingProblem<1, 1, (f64, f64)> for Mix {
    fn function(&self, t: Pt<1>) -> PeroxideResult<Pt<1>> {
        cdf_residual_mix(
            t,
            self.exp_lambda,
            self.erlang_lambda,
            self.erlang_k,
            self.quantile,
            self.mix_proportion,
        )
    }

    fn initial_guess(&self) -> (f64, f64) {
        // Constants
        let lambda_exp = self.exp_lambda;
        let lambda_erlang = self.erlang_lambda;
        let k = self.erlang_k;

        // Moments
        let mu = 1.0 / lambda_exp + (k as f64) / lambda_erlang;
        let var = 1.0 / (lambda_exp * lambda_exp) + (k as f64) / (lambda_erlang * lambda_erlang);
        let sigma = var.sqrt();

        // Computation with the standard normal estimation
        let z = normal_inverse_cdf(self.quantile);
        let lamda_min = lambda_exp.min(lambda_erlang);
        let t_exp = -(-self.quantile).ln_1p() / lamda_min;
        let t_norm = mu + (z + 6.0) * sigma;
        (0.0, t_exp.max(t_norm).max(0.0))
    }
}

/// Implementation with one float as the initial guess and an
/// implementation of the derivative function for use by the
/// NewtonMethod
impl RootFindingProblem<1, 1, f64> for Mix {
    fn function(&self, t: Pt<1>) -> PeroxideResult<Pt<1>> {
        cdf_residual_mix(
            t,
            self.exp_lambda,
            self.erlang_lambda,
            self.erlang_k,
            self.quantile,
            self.mix_proportion,
        )
    }

    fn initial_guess(&self) -> f64 {
        // Constants
        let lambda_exp = self.exp_lambda;
        let lambda_erlang = self.erlang_lambda;
        let k = self.erlang_k;

        // Moments
        let mu = 1.0 / lambda_exp + (k as f64) / lambda_erlang;
        let var = 1.0 / (lambda_exp * lambda_exp) + (k as f64) / (lambda_erlang * lambda_erlang);
        let sigma = var.sqrt();

        // Computation with the standard normal estimation
        let z = normal_inverse_cdf(self.quantile);
        let lamda_min = lambda_exp.min(lambda_erlang);
        let t_exp = -(-self.quantile).ln_1p() / lamda_min;
        let t_norm = mu + (z + 6.0) * sigma;
        t_exp.max(t_norm).max(0.0)
    }

    fn derivative(&self, t: Pt<1>) -> PeroxideResult<Jaco<1, 1>> {
        pdf_mix(
            t,
            self.exp_lambda,
            self.erlang_lambda,
            self.erlang_k,
            self.mix_proportion,
        )
    }
}

struct ExpAndErlang {
    exp_lambda: f64,
    erlang_lambda: f64,
    erlang_k: u64,
    quantile: f64,
}

/// Implementation with two floats as the initial guess for use by the
/// BisectMethod
impl RootFindingProblem<1, 1, (f64, f64)> for ExpAndErlang {
    fn function(&self, t: Pt<1>) -> PeroxideResult<Pt<1>> {
        cdf_residual_exp_and_erlang(
            t,
            self.exp_lambda,
            self.erlang_lambda,
            self.erlang_k,
            self.quantile,
        )
    }

    fn initial_guess(&self) -> (f64, f64) {
        // Constants
        let lambda_exp = self.exp_lambda;
        let lambda_erlang = self.erlang_lambda;
        let k = self.erlang_k;

        // Moments
        let mu = 1.0 / lambda_exp + (k as f64) / lambda_erlang;
        let var = 1.0 / (lambda_exp * lambda_exp) + (k as f64) / (lambda_erlang * lambda_erlang);
        let sigma = var.sqrt();

        // Computation with the standard normal estimation
        let z = normal_inverse_cdf(self.quantile);
        let lamda_min = lambda_exp.min(lambda_erlang);
        let t_exp = -(-self.quantile).ln_1p() / lamda_min;
        let t_norm = mu + (z + 6.0) * sigma;
        (0.0, t_exp.max(t_norm).max(0.0))
    }
}

/// Implementation with one float as the initial guess and an
/// implementation of the derivative function for use by the
/// NewtonMethod
impl RootFindingProblem<1, 1, f64> for ExpAndErlang {
    fn function(&self, t: Pt<1>) -> PeroxideResult<Pt<1>> {
        cdf_residual_exp_and_erlang(
            t,
            self.exp_lambda,
            self.erlang_lambda,
            self.erlang_k,
            self.quantile,
        )
    }

    fn initial_guess(&self) -> f64 {
        // Constants
        let lambda_exp = self.exp_lambda;
        let lambda_erlang = self.erlang_lambda;
        let k = self.erlang_k;

        // Moments
        let mu = 1.0 / lambda_exp + (k as f64) / lambda_erlang;
        let var = 1.0 / (lambda_exp * lambda_exp) + (k as f64) / (lambda_erlang * lambda_erlang);
        let sigma = var.sqrt();

        // Computation with the standard normal estimation
        let z = normal_inverse_cdf(self.quantile);
        let lamda_min = lambda_exp.min(lambda_erlang);
        let t_exp = -(-self.quantile).ln_1p() / lamda_min;
        let t_norm = mu + (z + 6.0) * sigma;
        t_exp.max(t_norm).max(0.0)
    }

    fn derivative(&self, t: Pt<1>) -> PeroxideResult<Jaco<1, 1>> {
        pdf_exp_and_erlang(t, self.exp_lambda, self.erlang_lambda, self.erlang_k)
    }
}

struct Erlang {
    lambda: f64,
    k: u64,
    quantile: f64,
}

/// Implementation with two floats as the initial guess for use by the
/// BisectMethod
impl RootFindingProblem<1, 1, (f64, f64)> for Erlang {
    fn function(&self, t: Pt<1>) -> PeroxideResult<Pt<1>> {
        cdf_residual_erlang(t, self.lambda, self.k, self.quantile)
    }

    fn initial_guess(&self) -> (f64, f64) {
        let mean = self.k as f64 / self.lambda;
        let std_dev = (self.k as f64).sqrt() / self.lambda;
        let normal_quantile = normal_inverse_cdf(self.quantile);
        (0.0, mean + (normal_quantile + 6.0) * std_dev)
    }
}

/// Implementation with one float as the initial guess and an
/// implementation of the derivative function for use by the
/// NewtonMethod
impl RootFindingProblem<1, 1, f64> for Erlang {
    fn function(&self, t: Pt<1>) -> PeroxideResult<Pt<1>> {
        cdf_residual_erlang(t, self.lambda, self.k, self.quantile)
    }

    fn initial_guess(&self) -> f64 {
        let mean = self.k as f64 / self.lambda;
        let std_dev = (self.k as f64).sqrt() / self.lambda;
        let normal_quantile = normal_inverse_cdf(self.quantile);
        mean + normal_quantile * std_dev
    }

    fn derivative(&self, t: Pt<1>) -> PeroxideResult<Jaco<1, 1>> {
        pdf_erlang(t, self.lambda, self.k)
    }
}

/// Computes the value of the inverse CDF of the Erlang distribution
/// at the desired quantile.
pub fn inverse_cdf(
    exp_lambda: Option<f64>,
    erlang_lambda: f64,
    erlang_k: u64,
    quantile: f64,
    mix_proportion: Option<f64>,
) -> PeroxideResult<Pt<1>> {
    match (exp_lambda, mix_proportion) {
        (Some(exp_lambda), Some(mix_proportion)) => {
            let problem = Mix {
                exp_lambda,
                erlang_lambda,
                erlang_k,
                quantile,
                mix_proportion,
            };
            let root_finder = BisectionMethod {
                max_iter: 100,
                tol: 1e-6,
            };
            root_finder.find(&problem)
        }
        (Some(exp_lambda), None) => {
            let problem = ExpAndErlang {
                exp_lambda,
                erlang_lambda,
                erlang_k,
                quantile,
            };
            let root_finder = BisectionMethod {
                max_iter: 100,
                tol: 1e-6,
            };
            root_finder.find(&problem)
        }
        (None, _) => {
            let problem = Erlang {
                lambda: erlang_lambda,
                k: erlang_k,
                quantile,
            };
            let root_finder = BisectionMethod {
                max_iter: 100,
                tol: 1e-6,
            };
            root_finder.find(&problem)
        }
    }
}
