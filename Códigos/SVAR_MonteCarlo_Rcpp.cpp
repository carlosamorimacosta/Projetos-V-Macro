
#include <RcppArmadillo.h>
#include <random>
#include <vector>
#include <cmath>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp14)]]

using namespace Rcpp;

struct VarFit {
  arma::mat B, Sigma, U, X, Ydep, companion;
  double max_root;
  int T, K, p;
};

arma::mat build_X(const arma::mat& Y, int p) {
  int T = Y.n_rows, K = Y.n_cols, Te = T - p;
  arma::mat X(Te, 1 + K * p, arma::fill::ones);

  for (int t = p; t < T; ++t) {
    int r = t - p;
    for (int lag = 1; lag <= p; ++lag) {
      X.submat(r, 1 + (lag - 1) * K, r, lag * K) = Y.row(t - lag);
    }
  }
  return X;
}

arma::mat build_Ydep(const arma::mat& Y, int p) {
  return Y.rows(p, Y.n_rows - 1);
}

arma::mat companion_from_B(const arma::mat& B, int K, int p) {
  arma::mat F(K * p, K * p, arma::fill::zeros);

  for (int lag = 0; lag < p; ++lag) {
    arma::mat block = B.rows(1 + lag * K, 1 + (lag + 1) * K - 1);
    arma::mat A = block.t();
    F.submat(0, lag * K, K - 1, (lag + 1) * K - 1) = A;
  }

  if (p > 1) {
    F.submat(K, 0, K * p - 1, K * (p - 1) - 1) =
      arma::eye(K * (p - 1), K * (p - 1));
  }
  return F;
}

VarFit fit_var_internal(const arma::mat& Y, int p) {
  if ((int)Y.n_rows <= p + 2) stop("Amostra insuficiente para estimar o VAR.");

  int T = Y.n_rows, K = Y.n_cols;
  arma::mat X = build_X(Y, p);
  arma::mat Ydep = build_Ydep(Y, p);

  arma::mat XtX = X.t() * X;
  arma::mat XtY = X.t() * Ydep;

  arma::mat B;
  bool ok = arma::solve(B, XtX, XtY, arma::solve_opts::likely_sympd);
  if (!ok) B = arma::pinv(XtX) * XtY;

  arma::mat U = Ydep - X * B;
  int dof = std::max(1, (int)Ydep.n_rows - (int)X.n_cols);

  arma::mat Sigma = (U.t() * U) / (double)dof;
  Sigma = 0.5 * (Sigma + Sigma.t());
  Sigma.diag() += 1e-10;

  arma::mat F = companion_from_B(B, K, p);
  arma::cx_vec eigval = arma::eig_gen(F);

  double max_root = 0.0;
  for (arma::uword i = 0; i < eigval.n_elem; ++i) {
    max_root = std::max(max_root, std::abs(eigval(i)));
  }

  VarFit out;
  out.B = B; out.Sigma = Sigma; out.U = U; out.X = X; out.Ydep = Ydep;
  out.companion = F; out.max_root = max_root; out.T = T; out.K = K; out.p = p;
  return out;
}

std::vector<arma::mat> get_A_mats(const arma::mat& B, int K, int p) {
  std::vector<arma::mat> A(p);
  for (int lag = 0; lag < p; ++lag) {
    arma::mat block = B.rows(1 + lag * K, 1 + (lag + 1) * K - 1);
    A[lag] = block.t();
  }
  return A;
}

std::vector<arma::mat> ma_mats(const arma::mat& B, int K, int p, int H) {
  std::vector<arma::mat> A = get_A_mats(B, K, p);
  std::vector<arma::mat> Psi(H + 1);
  Psi[0] = arma::eye(K, K);

  for (int h = 1; h <= H; ++h) {
    arma::mat cur(K, K, arma::fill::zeros);
    for (int lag = 1; lag <= std::min(p, h); ++lag) {
      cur += A[lag - 1] * Psi[h - lag];
    }
    Psi[h] = cur;
  }
  return Psi;
}

arma::mat lower_chol_safe(const arma::mat& Sigma) {
  arma::mat P;
  bool ok = arma::chol(P, Sigma, "lower");

  if (!ok) {
    arma::vec eigval;
    arma::mat eigvec;
    arma::eig_sym(eigval, eigvec, Sigma);
    eigval.transform([](double x) { return std::max(x, 1e-10); });
    arma::mat Sfix = eigvec * arma::diagmat(eigval) * eigvec.t();
    ok = arma::chol(P, Sfix, "lower");
    if (!ok) stop("Falha ao obter Cholesky da matriz de covariâncias.");
  }
  return P;
}

arma::vec orth_irf_one_response(
    const arma::mat& B,
    const arma::mat& Sigma,
    int K, int p, int H,
    int shock_idx, int response_idx,
    bool cumulative,
    double target_impact
) {
  std::vector<arma::mat> Psi = ma_mats(B, K, p, H);
  arma::mat P = lower_chol_safe(Sigma);
  arma::vec impact = P.col(shock_idx);

  if (std::isfinite(target_impact) && target_impact > 0.0) {
    double own_impact = impact(shock_idx);
    if (std::abs(own_impact) < 1e-12) stop("Impacto próprio do choque é praticamente zero.");
    impact *= target_impact / own_impact;
  }

  arma::vec out(H + 1, arma::fill::zeros);
  for (int h = 0; h <= H; ++h) {
    out(h) = arma::as_scalar(arma::rowvec(Psi[h].row(response_idx)) * impact);
  }

  if (cumulative) out = arma::cumsum(out);
  return out;
}

arma::mat simulate_var_residual_boot(
    const arma::mat& Y,
    const VarFit& fit,
    std::mt19937_64& rng
) {
  int T = fit.T, K = fit.K, p = fit.p;
  arma::mat Ys(T, K, arma::fill::zeros);
  Ys.rows(0, p - 1) = Y.rows(0, p - 1);

  arma::mat Uc = fit.U;
  arma::rowvec muU = arma::mean(Uc, 0);
  Uc.each_row() -= muU;

  std::uniform_int_distribution<int> uid(0, Uc.n_rows - 1);
  std::vector<arma::mat> A = get_A_mats(fit.B, K, p);
  arma::vec c = fit.B.row(0).t();

  for (int t = p; t < T; ++t) {
    arma::vec yt = c;
    for (int lag = 1; lag <= p; ++lag) {
      yt += A[lag - 1] * Ys.row(t - lag).t();
    }
    yt += Uc.row(uid(rng)).t();
    Ys.row(t) = yt.t();
  }
  return Ys;
}

arma::mat random_orthogonal(int K, std::mt19937_64& rng) {
  std::normal_distribution<double> nd(0.0, 1.0);
  arma::mat Z(K, K);

  for (int i = 0; i < K; ++i)
    for (int j = 0; j < K; ++j)
      Z(i, j) = nd(rng);

  arma::mat Q, R;
  arma::qr_econ(Q, R, Z);

  for (int j = 0; j < K; ++j) {
    double s = (R(j, j) >= 0.0) ? 1.0 : -1.0;
    Q.col(j) *= s;
  }
  return Q;
}

bool passes_sign_restrictions(
    const std::vector<arma::mat>& Psi,
    const arma::vec& impact,
    const IntegerVector& var_idx,
    const IntegerVector& h_idx,
    const IntegerVector& sign_vec,
    double tol
) {
  int Rn = var_idx.size();

  for (int r = 0; r < Rn; ++r) {
    int v = var_idx[r];
    int h = h_idx[r];
    int s = sign_vec[r];

    double val = arma::as_scalar(arma::rowvec(Psi[h].row(v)) * impact);

    if (s > 0 && !(val > tol)) return false;
    if (s < 0 && !(val < -tol)) return false;
  }
  return true;
}

// [[Rcpp::export]]
Rcpp::List fit_var_cpp(const arma::mat& Y, int p = 2) {
  VarFit fit = fit_var_internal(Y, p);

  return Rcpp::List::create(
    _["B"] = fit.B,
    _["Sigma"] = fit.Sigma,
    _["residuals"] = fit.U,
    _["companion"] = fit.companion,
    _["max_root"] = fit.max_root,
    _["n"] = fit.T,
    _["K"] = fit.K,
    _["p"] = fit.p
  );
}

// [[Rcpp::export]]
arma::vec recursive_irf_cpp(
    const arma::mat& Y,
    int p, int H,
    int shock_idx, int response_idx,
    bool cumulative = true,
    double target_impact = NA_REAL
) {
  VarFit fit = fit_var_internal(Y, p);

  return orth_irf_one_response(
    fit.B, fit.Sigma, fit.K, p, H,
    shock_idx, response_idx, cumulative, target_impact
  );
}

// [[Rcpp::export]]
Rcpp::List residual_bootstrap_svar_cpp(
    const arma::mat& Y,
    int p = 2,
    int H = 24,
    int B = 20000,
    int shock_idx = 0,
    int response_idx = 4,
    bool cumulative = true,
    double target_impact = NA_REAL,
    double stability_cutoff = 0.999999,
    int seed = 20261007
) {
  VarFit base = fit_var_internal(Y, p);

  arma::vec point = orth_irf_one_response(
    base.B, base.Sigma, base.K, p, H,
    shock_idx, response_idx, cumulative, target_impact
  );

  arma::mat draws(B, H + 1, arma::fill::value(NA_REAL));
  std::mt19937_64 rng(seed);

  int accepted = 0, unstable = 0, failed = 0;

  for (int b = 0; b < B; ++b) {
    try {
      arma::mat Ys = simulate_var_residual_boot(Y, base, rng);
      VarFit fb = fit_var_internal(Ys, p);

      if (!std::isfinite(fb.max_root) || fb.max_root >= stability_cutoff) {
        unstable++;
        continue;
      }

      arma::vec irf = orth_irf_one_response(
        fb.B, fb.Sigma, fb.K, p, H,
        shock_idx, response_idx, cumulative, target_impact
      );

      draws.row(accepted) = irf.t();
      accepted++;
    } catch (...) {
      failed++;
    }
  }

  arma::mat kept;
  if (accepted > 0) kept = draws.rows(0, accepted - 1);
  else kept.set_size(0, H + 1);

  return Rcpp::List::create(
    _["point_irf"] = point,
    _["draws"] = kept,
    _["requested"] = B,
    _["accepted"] = accepted,
    _["unstable"] = unstable,
    _["failed"] = failed,
    _["acceptance_rate"] = (double)accepted / (double)B,
    _["base_max_root"] = base.max_root
  );
}

// [[Rcpp::export]]
Rcpp::List bets_measurement_uncertainty_cpp(
    const arma::mat& Y,
    int p = 2,
    int H = 24,
    int M = 10000,
    int bets_idx = 0,
    int response_idx = 4,
    double noise_sd_fraction = 0.10,
    bool cumulative = true,
    double target_impact = NA_REAL,
    double stability_cutoff = 0.999999,
    int seed = 20261008
) {
  int T = Y.n_rows, K = Y.n_cols;
  if (bets_idx < 0 || bets_idx >= K) stop("bets_idx inválido.");

  arma::vec bets = Y.col(bets_idx);
  double sdb = arma::stddev(bets);
  if (!std::isfinite(sdb) || sdb <= 0.0) stop("Desvio-padrão de Bets inválido.");

  double noise_sd = noise_sd_fraction * sdb;

  arma::mat draws(M, H + 1, arma::fill::value(NA_REAL));
  std::mt19937_64 rng(seed);
  std::normal_distribution<double> nd(0.0, noise_sd);

  int accepted = 0, unstable = 0, failed = 0;

  for (int m = 0; m < M; ++m) {
    arma::mat Ym = Y;
    for (int t = 0; t < T; ++t) Ym(t, bets_idx) += nd(rng);

    try {
      VarFit fm = fit_var_internal(Ym, p);

      if (!std::isfinite(fm.max_root) || fm.max_root >= stability_cutoff) {
        unstable++;
        continue;
      }

      arma::vec irf = orth_irf_one_response(
        fm.B, fm.Sigma, K, p, H,
        bets_idx, response_idx, cumulative, target_impact
      );

      draws.row(accepted) = irf.t();
      accepted++;
    } catch (...) {
      failed++;
    }
  }

  arma::mat kept;
  if (accepted > 0) kept = draws.rows(0, accepted - 1);
  else kept.set_size(0, H + 1);

  return Rcpp::List::create(
    _["draws"] = kept,
    _["requested"] = M,
    _["accepted"] = accepted,
    _["unstable"] = unstable,
    _["failed"] = failed,
    _["acceptance_rate"] = (double)accepted / (double)M,
    _["noise_sd_fraction"] = noise_sd_fraction,
    _["noise_sd"] = noise_sd
  );
}

// [[Rcpp::export]]
Rcpp::List sign_restrictions_cpp(
    const arma::mat& Y,
    int p,
    int H,
    IntegerVector restrict_var_idx,
    IntegerVector restrict_h_idx,
    IntegerVector restrict_sign,
    int response_idx = 4,
    int n_accept = 5000,
    int max_rotations = 500000,
    bool cumulative_response = true,
    double target_impact = NA_REAL,
    double tol = 1e-10,
    int seed = 20261009
) {
  if (
    restrict_var_idx.size() != restrict_h_idx.size() ||
    restrict_var_idx.size() != restrict_sign.size()
  ) stop("Vetores de restrições devem ter o mesmo comprimento.");

  VarFit fit = fit_var_internal(Y, p);
  int K = fit.K;

  std::vector<arma::mat> Psi = ma_mats(fit.B, K, p, H);
  arma::mat P = lower_chol_safe(fit.Sigma);

  arma::mat draws(n_accept, H + 1, arma::fill::value(NA_REAL));
  arma::mat impact_draws(n_accept, K, arma::fill::value(NA_REAL));

  std::mt19937_64 rng(seed);

  int accepted = 0, rotations = 0;

  while (accepted < n_accept && rotations < max_rotations) {
    rotations++;

    arma::mat Q = random_orthogonal(K, rng);
    arma::mat B0 = P * Q;

    for (int j = 0; j < K && accepted < n_accept; ++j) {
      arma::vec impact = B0.col(j);

      if (impact(0) < 0.0) impact *= -1.0;

      if (!passes_sign_restrictions(
        Psi, impact,
        restrict_var_idx, restrict_h_idx, restrict_sign, tol
      )) continue;

      if (std::isfinite(target_impact) && target_impact > 0.0) {
        double own = impact(0);
        if (std::abs(own) < 1e-12) continue;
        impact *= target_impact / own;
      }

      arma::vec resp(H + 1, arma::fill::zeros);

      for (int h = 0; h <= H; ++h) {
        resp(h) = arma::as_scalar(arma::rowvec(Psi[h].row(response_idx)) * impact);
      }

      if (cumulative_response) resp = arma::cumsum(resp);

      draws.row(accepted) = resp.t();
      impact_draws.row(accepted) = impact.t();
      accepted++;
    }
  }

  arma::mat kept_draws, kept_impacts;
  if (accepted > 0) {
    kept_draws = draws.rows(0, accepted - 1);
    kept_impacts = impact_draws.rows(0, accepted - 1);
  } else {
    kept_draws.set_size(0, H + 1);
    kept_impacts.set_size(0, K);
  }

  return Rcpp::List::create(
    _["draws"] = kept_draws,
    _["impact_vectors"] = kept_impacts,
    _["accepted"] = accepted,
    _["rotations"] = rotations,
    _["acceptance_rate_per_rotation"] =
      (rotations > 0 ? (double)accepted / (double)rotations : NA_REAL),
    _["base_max_root"] = fit.max_root
  );
}
