#Install and load the libraries
library("rstan")
library("rstudioapi")
stanmodelcode= '
data {
  int<lower=0> N;     // Number of data rows
 vector[N] Ag;        // Ag input (kg/km2/yr)
 vector[N] Urb_frt;   //Urban fertilizer input (kg/km2/yr)
 vector[N] Urb_wst;   //Urban human waste input (kg/km2/yr)
 vector[N] S;         // SoilP (ppm)
 vector[N] W;         // Water fraction
 vector[N] P;         // Prec (mm)
vector[N] y;          // Response vector
int  wsd [N];         //Ecoregion number for each obs (watershed/year)
int  wsd_tot;         //Total number of watershed/years
int ecoreg [N];       //Ecoregion number for each obs (watershed/year)
int  econum;          //Unique number of regions 
}
parameters {
 //real alpha0;                                 //Intercept
  real <lower=0, upper = 1> betaAg;             //Agriculture EC (unitless)
  real <lower=0, upper = 1> betaUrb_frt;        //Urban fertilizer EC (unitless)
  real <lower=0, upper = 1> betaUrb_wst;        //Urban human waste EC (unitless)
  real <lower=0, upper = 1>betaS;               //soil EC (kg/(ppm km2 yr))
  vector <lower=0, upper = 2> [econum] delta;   //EC-adjustment factor (unitless)
  real <lower=0, upper = 8>  gammaAg;           //Agricultural input PIC (unitless)
  real <lower=0, upper = 5>gammaUrb_frt;        //Urban fertilizer PIC (unitless)
  real <lower=0, upper = 5>gammaUrb_wst;        //Urban human waste PIC (unitless)
 real <lower=0, upper = 5>  gammaS;             //Soil PIC (unitless)
 vector <lower=0, upper = 2> [econum] psi;      //PIC-adjustment factor (unitless)
  real <lower=0, upper = 20>  k;                //waterbody loss rate (unitless)
  real <lower=0, upper = 1>sigma_psi;           //PIC-adjustment factor SD
  real <lower=0, upper = 1>sigma_delta;         //EC-adjustment factor SD
  real<lower=0> sigma_wsd;                      //Random effect SD
  real<lower=0> sigma;                          //Model residual SD 
vector [wsd_tot] alpha;

}
model {
  vector[N] yhat; //Total loadings after retention
  vector[N] ly;
  vector[N] lyb;
  ly=log(y+1);
  yhat=(betaAg*delta[ecoreg].*Ag.*pow(P,gammaAg*psi[ecoreg])+betaUrb_frt*delta[ecoreg].*Urb_frt.*pow(P,gammaUrb_frt*psi[ecoreg])+betaUrb_wst*delta[ecoreg].*Urb_wst.*pow(P,gammaUrb_wst*psi[ecoreg])+betaS*delta[ecoreg].*S.*pow(P,gammaS*psi[ecoreg])).* exp(-W*k) ;
  lyb=log(yhat+1)+alpha[wsd];
  ly ~ normal(lyb, sigma);  // likelihood
//Priors 
  k~ normal(6,3);
  betaAg~ normal(.046,.021);
  betaUrb_frt~ normal(.061,.04);
  betaUrb_wst~ normal(.5,.2);
  betaS~ normal(.026,.012);
   sigma_wsd ~ normal(0,1);     
  alpha ~ normal(0,sigma_wsd); 
 gammaAg~ normal(3,.5);
  gammaUrb_frt~ normal(1,.5);
  gammaUrb_wst~ normal(0,.5);
  gammaS~ normal(2,.5);
psi~ normal(1,sigma_psi);
delta~ normal(1,sigma_delta);
sigma_psi~ normal(0,.1);
sigma_delta~ normal(0,.1);
}
'

data <- readRDS("./data.rds") #load annual input data set, if necessary
#run the model (data is the list of data sets. For other parameters, return to the function description)
model = stan(model_code=stanmodelcode, data=data, iter=iter, 
                    warmup=warmup, thin= thin, chains=3,cores=3,
                    control = list(adapt_delta =adapt_delta ,max_treedepth =max_treedepth ))

