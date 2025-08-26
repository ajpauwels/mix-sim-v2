use std::f64::consts::E;

use peroxide::fuga::{
    anyhow::Result as PeroxideResult, inv_erf, BisectionMethod, Jaco, Pt, RootFinder,
    RootFindingProblem,
};

struct Erlang {
    k: i32,
    lambda: f64,
    quantile: f64,
}

impl Erlang {
    fn pdf(&self, x: Pt<1>) -> PeroxideResult<Jaco<1, 1>> {
        let x = x[0];
        let result = self.lambda.powi(self.k) * x.powi(self.k - 1) * E.powf(-self.lambda * x)
            / (factorial(self.k.try_into().unwrap()) as f64);
        Ok([[result]])
    }

    fn cdf(&self, x: Pt<1>) -> PeroxideResult<Pt<1>> {
        let x = x[0];
        let mut sum = 0.0;
        for n in 0..self.k {
            sum += (1.0 / factorial(n.try_into().unwrap()) as f64) * (self.lambda * x).powi(n);
        }
        let total = 1.0 - E.powf(-self.lambda * x) * sum;
        Ok([total])
    }

    fn cdf_residual(&self, x: Pt<1>) -> PeroxideResult<Pt<1>> {
        let cdf = self.cdf(x)?[0];
        Ok([cdf - self.quantile])
    }
}

/// Implementation with two floats as the initial guess for use by the
/// BisectMethod
impl RootFindingProblem<1, 1, (f64, f64)> for Erlang {
    fn function(&self, x: Pt<1>) -> PeroxideResult<Pt<1>> {
        self.cdf_residual(x)
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
/// NetwtonMethod
impl RootFindingProblem<1, 1, f64> for Erlang {
    fn function(&self, x: Pt<1>) -> PeroxideResult<Pt<1>> {
        self.cdf_residual(x)
    }

    fn initial_guess(&self) -> f64 {
        let mean = self.k as f64 / self.lambda;
        let std_dev = (self.k as f64).sqrt() / self.lambda;
        let normal_quantile = normal_inverse_cdf(self.quantile);
        mean + normal_quantile * std_dev
    }

    fn derivative(&self, x: Pt<1>) -> PeroxideResult<Jaco<1, 1>> {
        self.pdf(x)
    }
}

/// Computes the factorial of a positive integer
fn factorial(n: u64) -> u64 {
    if n <= 1 {
        1
    } else {
        let mut curr = 1;
        for i in 1..(n + 1) {
            curr *= i;
        }
        curr
    }
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

/// Computes the value of the inverse CDF of the Erlang distribution
/// at the desired quantile.
pub fn erlang_inverse_cdf(k: i32, lambda: f64, quantile: f64) -> PeroxideResult<Pt<1>> {
    if quantile <= 0.0 || quantile >= 1.0 {
        panic!("Inverse cdf of the Erlang distribution can only be computed if the desired quantile q is 0.0 < q < 1.0");
    } else {
        let problem = Erlang {
            k,
            lambda,
            quantile,
        };
        let root_finder = BisectionMethod {
            max_iter: 100,
            tol: 1e-6,
        };
        root_finder.find(&problem)
    }
}
