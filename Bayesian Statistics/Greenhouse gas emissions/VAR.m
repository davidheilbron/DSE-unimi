%% Gibbs sampler on a VAR model
clear; clc; close all;
% Set the seed 
rand('seed',100);
randn('seed',100)

% Set the lag of the VAR
p = 3;  % if p > 4, need to change Y0 and Y below

% Gibbs sampler iterations
nsim   = 20000;
burnin = 1000;

% Load data
Y_tmp = readtable('bayes_data_8.csv');
Yraw  = table2array(Y_tmp(:,2:end));
Dates = Y_tmp(:,1);
Y0 = Yraw(1:4,:); % save the first 4 obs as the initial conditions
Y  = Yraw(5:end,:);

[T,n] = size(Y);
y     = reshape(Y',T*n,1);
k     = n*p + 1; % # of coefficients in each equation
n_hz  = 5;     % # of horizons for IRs 
    
% Prior hyperparameters
nu0 = n + 3; S0 = eye(n); % Inverse Wishart
beta0 = zeros(n*k,1);
% precision for coefficients = 1; for intercepts = 1/10
tmp = ones(k*n,1);  tmp(1:p*n+1:k*n) = 1/10;  
iVbeta = sparse(1:k*n,1:k*n,tmp);

% Create the lagged variable
tmpY    = [Y0(end-p+1:end,:); Y];
X_tilde = zeros(T,n*p); 
for i=1:p
    X_tilde(:,(i-1)*n+1:i*n) = tmpY(p-i+1:end-i,:);
end
X_tilde = [ones(T,1) X_tilde];
X       = SURform2(X_tilde,n); 

% Storage of variables
store_Sig  = zeros(nsim,n,n); 
store_beta = zeros(nsim,k*n); 
store_yIR  = zeros(n_hz,n); 

% Initialize the chain 
beta = (X'*X)\(X'*y);
e    = reshape(y - X*beta,n,T);
Sig  = e*e'/T;    
iSig = Sig\speye(n);

%% Gibbs sampler 
tic;
for isim = 1:nsim + burnin  
    % Posterior of beta \sim Normal
    XiSig    = X'*kron(speye(T),iSig);
    XiSigX   = XiSig*X;
    Kbeta    = iVbeta + XiSigX; % posterior variance
    beta_hat = Kbeta\(iVbeta*beta0 + XiSig*y); % posterior mean
    beta     = beta_hat + chol(Kbeta,'lower')'\randn(n*k,1);

    % posterior of Sigma \sim iWishart
    e    = reshape(y - X*beta,n,T);    
    Sig  = iwishrnd(S0 + e*e',nu0 + T);
    iSig = Sig\speye(n);
    
    % Count the number of iterations
    if ~mod(isim,1000)
      disp(['Iteration: ',num2str(isim),' of ',num2str(nsim + burnin),'. Elapsed time is ',num2str(toc),' seconds.']);
    end 
    
    % store the parameters
    if isim > burnin
        isave = isim - burnin;
        store_beta(isave,:)  = beta';
        store_Sig(isave,:,:) = Sig;
        
        % compute impulse-responses
        CSig = chol(Sig,'lower');
        % 100 basis pts rather than 1 std. dev.
        shock = [0; 1; 0]/CSig(n,n); 
        yIR   = construct_IR(beta,Sig,n_hz,shock);
        store_yIR = store_yIR + yIR;        
    end
end

%% Graphical part
beta_hat = mean(store_beta)';
Sig_hat  = squeeze(mean(store_Sig));
yIR_hat  = store_yIR/nsim;

% Figure for betas
fs = 22;
figure
plot(store_beta(:,1))
title('Chain for B0 -- with burn-in')
set(gca,'FontSize',fs);

figure
autocorr(store_beta(:,1))
title('Autocorrelation for B0  -- with burn-in')
set(gca,'FontSize',fs);

% Figure for IRF
figure;
subplot(1,3,1);
plot(yIR_hat(:,1)); box off; xlim([1 n_hz]);
title('Response: net greenhouse, Impulse: gas imports');
subplot(1,3,2);
plot(yIR_hat(:,2)); box off; xlim([1 n_hz]);
title('Response: industrial prod, Impulse: gas imports');
subplot(1,3,3);
plot(yIR_hat(:,3)); box off; xlim([1 n_hz]);
title('Response: gas imports, Impulse: gas imports');
set(gcf,'Position',[100 100 800 300]);
