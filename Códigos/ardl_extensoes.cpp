// ============================================================================
// PROJETO V - Extensoes computacionais do ARDL/SVAR(2)
// Rcpp + RcppArmadillo. Compilar SOMENTE via Rcpp::sourceCpp().
// ============================================================================
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp17)]]
#include <RcppArmadillo.h>
#include <cmath>
#include <algorithm>
#include <complex>
#include <limits>

using namespace Rcpp;
using namespace arma;

namespace {

void valida(const vec& y, const vec& bets, const vec& ibc,
            const vec& ipca, const vec& selic) {
  const uword n = y.n_elem;
  if(n < 20 || bets.n_elem != n || ibc.n_elem != n ||
     ipca.n_elem != n || selic.n_elem != n) {
    stop("As cinco series devem ter o mesmo tamanho e pelo menos 20 meses.");
  }
  if(!y.is_finite() || !bets.is_finite() || !ibc.is_finite() ||
     !ipca.is_finite() || !selic.is_finite()) {
    stop("As series possuem valores NA, NaN ou infinitos.");
  }
}

// Colunas: intercepto, inad_L1, inad_L2, bets_L1, bets_L2,
//          ibc_L1, ibc_L2, ipca_L1, ipca_L2, selic_L1, selic_L2.
mat cria_X(const vec& y, const vec& bets, const vec& ibc,
           const vec& ipca, const vec& selic) {
  const uword n = y.n_elem;
  mat X(n - 2, 11, fill::ones);
  for(uword t=2; t<n; ++t) {
    uword i = t-2;
    X(i,1) = y(t-1); X(i,2) = y(t-2);
    X(i,3) = bets(t-1); X(i,4) = bets(t-2);
    X(i,5) = ibc(t-1);  X(i,6) = ibc(t-2);
    X(i,7) = ipca(t-1); X(i,8) = ipca(t-2);
    X(i,9) = selic(t-1); X(i,10) = selic(t-2);
  }
  return X;
}

vec ols(const mat& X, const vec& y) {
  vec b;
  // QR de minimos quadrados com fallback SVD se matriz pouco condicionada.
  bool ok = arma::solve(b, X, y);
  if(!ok || !b.is_finite()) b = arma::pinv(X) * y;
  return b;
}

// Covariancia HAC Bartlett / Newey-West com ajuste n/(n-k).
// Corresponde ao NeweyWest(prewhite=FALSE, adjust=TRUE, lag=L)
// para um modelo lm usual sem outras correcoes.
mat hac_vcov(const mat& X, const vec& resid, int L) {
  const uword n = X.n_rows;
  const uword k = X.n_cols;
  mat S(k,k,fill::zeros);
  for(uword t=0; t<n; ++t) {
    vec s = X.row(t).t() * resid(t);
    S += s * s.t();
  }
  for(int lag=1; lag<=L; ++lag) {
    const double peso = 1.0 - double(lag)/double(L+1);
    for(uword t=(uword)lag; t<n; ++t) {
      vec st = X.row(t).t() * resid(t);
      vec sl = X.row(t-lag).t() * resid(t-lag);
      S += peso * (st*sl.t() + sl*st.t());
    }
  }
  mat invxx = arma::pinv(X.t()*X);
  mat V = invxx*S*invxx;
  if(n>k) V *= double(n)/double(n-k);
  return arma::symmatu(V);
}

double wald_bets(const mat& X, const vec& y, const vec& b, int L) {
  vec e = y - X*b;
  mat V = hac_vcov(X,e,L);
  mat VV = V.submat(3,3,4,4);
  if(!VV.is_finite()) return NA_REAL;
  vec bb = b.subvec(3,4);
  mat invv = arma::pinv(VV);
  double w = arma::as_scalar(bb.t()*invv*bb);
  return (std::isfinite(w) && w>=0) ? w : NA_REAL;
}

vec bloco_circular(const vec& e, int bloco) {
  const int n = (int)e.n_elem;
  vec out(n);
  int i=0;
  while(i<n) {
    const int ini = (int)std::floor(R::runif(0.0, (double)n));
    for(int j=0; j<bloco && i<n; ++j,++i) out(i)=e((ini+j)%n);
  }
  return out;
}

// Simula dados sob parametros estimados, mantendo X exogeno fixo.
vec simular_y(const vec& y0, const vec& bets, const vec& ibc,
              const vec& ipca, const vec& selic,
              const vec& b, const vec& e_b) {
  const uword n = y0.n_elem;
  vec ys(n,fill::zeros);
  ys(0)=y0(0); ys(1)=y0(1);
  for(uword t=2;t<n;++t) {
    ys(t) = b(0) + b(1)*ys(t-1) + b(2)*ys(t-2)
      + b(3)*bets(t-1) + b(4)*bets(t-2)
      + b(5)*ibc(t-1)  + b(6)*ibc(t-2)
      + b(7)*ipca(t-1) + b(8)*ipca(t-2)
      + b(9)*selic(t-1)+ b(10)*selic(t-2)
      + e_b(t-2);
  }
  return ys;
}

vec multiplicadores(const vec& b,int H) {
  vec m(H+1, fill::zeros);
  const double p1=b(1), p2=b(2);
  const double q1=b(3), q2=b(4);
  for(int h=1; h<=H; ++h) {
    double v=0.0;
    if(h==1) v+=q1;
    if(h==2) v+=q2;
    v+=p1*m(h-1);
    if(h>=2) v+=p2*m(h-2);
    m(h)=v;
  }
  return m;
}

// Raizes de r^2-phi1*r-phi2: estabilidade quando modulos < 1.
bool estavel(const vec& b) {
  std::complex<double> D(b(1)*b(1)+4*b(2),0.0);
  std::complex<double> r1=(b(1)+std::sqrt(D))/2.0;
  std::complex<double> r2=(b(1)-std::sqrt(D))/2.0;
  return std::abs(r1)<1.0 && std::abs(r2)<1.0;
}

inline double soft(double z,double t) {
  if(z>t) return z-t;
  if(z< -t) return z+t;
  return 0.0;
}

// Caminho para Ridge (alpha=0) ou ENet (0<alpha<=1).
// Cada linha da saida: intercepto e 10 coeficientes na escala original.
mat enet_path(const mat& X,const vec& y,const vec& lambdas,
              double alpha,int maxit,double tol) {
  const uword n=X.n_rows, k=X.n_cols;
  rowvec mu=arma::mean(X,0);
  rowvec sd(k,fill::ones);
  mat Z=X;
  for(uword j=0;j<k;++j) {
    Z.col(j)-=mu(j);
    double s=std::sqrt(arma::dot(Z.col(j),Z.col(j))/double(n));
    if(s>1e-12) { sd(j)=s; Z.col(j)/=s; }
    else { sd(j)=1.0; Z.col(j).zeros(); }
  }
  const double ym=arma::mean(y);
  vec yc=y-ym;
  mat G=Z.t()*Z/double(n);
  vec z=Z.t()*yc/double(n);
  vec theta(k,fill::zeros);
  mat out(lambdas.n_elem,k+1,fill::zeros);
  for(uword a=0;a<lambdas.n_elem;++a) {
    const double lam=lambdas(a);
    for(int iter=0;iter<maxit;++iter) {
      double max_d=0.0;
      for(uword j=0;j<k;++j) {
        double off=arma::dot(G.row(j),theta)-G(j,j)*theta(j);
        double numer=z(j)-off;
        double novo=soft(numer,lam*alpha)/(G(j,j)+lam*(1-alpha));
        if(!std::isfinite(novo)) novo=0.0;
        max_d=std::max(max_d,std::abs(novo-theta(j)));
        theta(j)=novo;
      }
      if(max_d<tol) break;
    }
    rowvec borig=(theta/sd.t()).t();
    out(a,0)=ym-arma::dot(mu,borig);
    for(uword j=0;j<k;++j) out(a,j+1)=borig(j);
  }
  return out;
}

} // namespace

// [[Rcpp::export]]
Rcpp::List ardl_ols_hac_cpp(const arma::vec& y,const arma::vec& bets,
                            const arma::vec& ibc,const arma::vec& ipca,
                            const arma::vec& selic,int L=2) {
  valida(y,bets,ibc,ipca,selic);
  mat X=cria_X(y,bets,ibc,ipca,selic);
  vec yy=y.subvec(2,y.n_elem-1);
  vec b=ols(X,yy);
  mat V=hac_vcov(X,yy-X*b,L);
  return List::create(_["coef"]=b,_["V_HAC"]=V,
    _["Wald_Bets"]=wald_bets(X,yy,b,L),
    _["residuos"]=yy-X*b,_["estavel"]=estavel(b));
}

// [[Rcpp::export]]
Rcpp::List multiplicadores_cpp(const arma::vec& coef,int H=24) {
  if(coef.n_elem!=11 || H<1 || H>240) stop("Coef deve ter 11 elementos; H entre 1 e 240.");
  vec m=multiplicadores(coef,H);
  return List::create(_["d_inad"]=m,_["inad_nivel"]=arma::cumsum(m));
}

// Bootstrap recursivo com blocos moveis circulares dos residuos.
// 1. Amostras irrestritas: IC para coeficientes e multiplicadores.
// 2. Amostras sob H0 beta_bets_L1=beta_bets_L2=0: Wald HAC bootstrap.
// [[Rcpp::export]]
Rcpp::List bootstrap_ardl_cpp(const arma::vec& y,const arma::vec& bets,
                              const arma::vec& ibc,const arma::vec& ipca,
                              const arma::vec& selic,int B=5000,
                              int block_size=4,int H=24,int hac_lag=2) {
  Rcpp::RNGScope scope;
  valida(y,bets,ibc,ipca,selic);
  if(B<100 || block_size<1 || block_size>(int)y.n_elem-2 || H<1 || H>120)
    stop("Parametros de bootstrap invalidos.");
  mat X=cria_X(y,bets,ibc,ipca,selic);
  vec yy=y.subvec(2,y.n_elem-1);
  vec b=ols(X,yy);
  vec eu=yy-X*b;
  eu-=arma::mean(eu);
  // Modelo restrito sem as colunas de bets.
  uvec cols={0,1,2,5,6,7,8,9,10};
  mat XR=X.cols(cols);
  vec br=ols(XR,yy);
  vec b0(11,fill::zeros);
  b0.elem(cols)=br;
  vec e0=yy-XR*br;
  e0-=arma::mean(e0);
  const double stat_obs=wald_bets(X,yy,b,hac_lag);
  const int k=(int)b.n_elem;
  mat b_boot(B,k,fill::zeros);
  mat resp_boot(B,H+1,fill::zeros);
  mat nivel_boot(B,H+1,fill::zeros);
  vec stat_null(B,fill::zeros);
  LogicalVector stable(B);
  for(int it=0;it<B;++it) {
    vec yb=simular_y(y,bets,ibc,ipca,selic,b,bloco_circular(eu,block_size));
    mat Xb=cria_X(yb,bets,ibc,ipca,selic);
    vec by=yb.subvec(2,yb.n_elem-1);
    vec bi=ols(Xb,by);
    b_boot.row(it)=bi.t();
    stable[it]=estavel(bi);
    vec m=multiplicadores(bi,H);
    resp_boot.row(it)=m.t();
    nivel_boot.row(it)=arma::cumsum(m).t();

    vec yn=simular_y(y,bets,ibc,ipca,selic,b0,bloco_circular(e0,block_size));
    mat Xn=cria_X(yn,bets,ibc,ipca,selic);
    vec ny=yn.subvec(2,yn.n_elem-1);
    vec bn=ols(Xn,ny);
    stat_null(it)=wald_bets(Xn,ny,bn,hac_lag);
  }
  int excess=0, valid=0;
  for(int it=0;it<B;++it) {
    if(std::isfinite(stat_null(it))) {
      ++valid;
      if(stat_null(it)>=stat_obs) ++excess;
    }
  }
  double p_boot=(valid>0 && std::isfinite(stat_obs)) ?
    double(excess+1)/double(valid+1) : NA_REAL;
  return List::create(_["coef_original"]=b, _["wald_original"]=stat_obs,
    _["wald_null"]=stat_null, _["p_boot_wald"]=p_boot,
    _["coef_boot"]=b_boot, _["resp_boot"]=resp_boot,
    _["nivel_boot"]=nivel_boot, _["estavel_boot"]=stable,
    _["B_validos_nulo"]=valid);
}

// Monte Carlo de perfis mensais de GGR REAL (indice reconstruido).
// Os totais POR ANO do indice real reconstruido sao preservados exatamente.
// sigma controla incerteza de log-nivel mensal, nao e estimado dos dados.
// rho determina persistencia AR(1) das perturbacoes dentro de cada ano.
// [[Rcpp::export]]
Rcpp::List monte_carlo_bets_cpp(const arma::vec& y,const arma::vec& bets,
                                const arma::vec& ibc,const arma::vec& ipca,
                                const arma::vec& selic,
                                const Rcpp::IntegerVector& years,
                                int B=2000,double sigma=0.10,double rho=0.5,
                                int H=24) {
  Rcpp::RNGScope scope;
  valida(y,bets,ibc,ipca,selic);
  const int n=(int)y.n_elem;
  if(years.size()!=n || B<100 || sigma<0 || sigma>1 || std::abs(rho)>=1 || H<1)
    stop("Parametros do Monte Carlo invalidos.");
  vec level0(n,fill::ones);
  // A escala inicial arbitraria se cancela na preservacao dos totais anuais.
  for(int t=1;t<n;++t) level0(t)=level0(t-1)*std::exp(bets(t)/100.0);
  if(!level0.is_finite()) stop("Overflow ao reconstruir os niveis de Bets.");
  // A serie deve estar em ordem cronologica e os anos serem consecutivos por bloco.
  std::vector<std::pair<int,int>> ranges;
  int ini=0;
  while(ini<n) {
    int fim=ini;
    while(fim+1<n && years[fim+1]==years[ini]) ++fim;
    ranges.push_back({ini,fim});
    ini=fim+1;
  }
  mat betas(B,2,fill::zeros);
  vec soma(B,fill::zeros), efeito24(B,fill::zeros);
  LogicalVector stable(B);
  mat amostras(n,4,fill::zeros);
  amostras.col(0)=level0;
  mat amostras_bets(n,4,fill::zeros);
  amostras_bets.col(0)=bets;
  for(int it=0;it<B;++it) {
    vec lv(n,fill::zeros);
    for(auto r : ranges) {
      const int a=r.first, z=r.second;
      double prev=0.0, sum_new=0.0, sum_orig=0.0;
      const double innov_sd=std::sqrt(1-rho*rho);
      for(int t=a;t<=z;++t) {
        prev=(t==a) ? R::rnorm(0.0,1.0) :
          rho*prev + innov_sd*R::rnorm(0.0,1.0);
        lv(t)=level0(t)*std::exp(sigma*prev);
        sum_new+=lv(t); sum_orig+=level0(t);
      }
      const double scale=sum_orig/sum_new;
      for(int t=a;t<=z;++t) lv(t)*=scale;
    }
    vec bsim=bets;
    for(int t=1;t<n;++t) bsim(t)=100.0*std::log(lv(t)/lv(t-1));
    // Primeiro crescimento mantido (anterior a amostra nao disponivel).
    mat Xs=cria_X(y,bsim,ibc,ipca,selic);
    vec by=y.subvec(2,y.n_elem-1);
    vec coef=ols(Xs,by);
    betas(it,0)=coef(3);
    betas(it,1)=coef(4);
    soma(it)=coef(3)+coef(4);
    efeito24(it)=arma::sum(multiplicadores(coef,H));
    stable[it]=estavel(coef);
    if(it<3) { amostras.col(it+1)=lv; amostras_bets.col(it+1)=bsim; }
  }
  return List::create(_["beta_bets"]=betas,_["soma_coef"]=soma,
    _["efeito_nivel_H"]=efeito24,_["estavel"]=stable,
    _["exemplos_nivel"]=amostras,_["exemplos_crescimento"]=amostras_bets);
}

// Elastic Net/Ridge por coordenadas ciclicas.
// X nao deve incluir intercepto; este sera calculado e nao penalizado.
// Lambdas DESCENDENTES sao recomendados (warm starts).
// [[Rcpp::export]]
arma::mat elastic_net_path_cpp(const arma::mat& X,const arma::vec& y,
                               const arma::vec& lambda,double alpha,
                               int max_iter=10000,double tol=1e-9) {
  if(X.n_rows!=y.n_elem || X.n_rows<10 || X.n_cols<1 ||
     lambda.n_elem<1 || alpha<0 || alpha>1 ||
     !X.is_finite() || !y.is_finite() ||
     arma::any(lambda<0)) stop("Entrada invalida para Elastic Net/Ridge.");
  return enet_path(X,y,lambda,alpha,max_iter,tol);
}
