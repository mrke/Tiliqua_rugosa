%% <../run_my_pet.m *run_my_pet*>
% created by Michael Kearney 2015/04/22
% modified 

%clear all; 
global pets

% species names
pets = {'Tiliqua_rugosa'};

% See estim_options for more options
estim_options('default'); % runs estimation, uses nmregr method and filter
                          % prints results, does not write file, does not produce html
% 'method':           'nm' - use Nelder-Mead method (default); 'no' - do not estimate;
% 'pars_init_method': 0 - get initial estimates from automatized computation (default)
%                     1 - read initial estimates from .mat file 
%                     2 - read initial estimates from pars_init file 
% 'results_output':   0 - prints results to screen; (default)
%                     1 - prints results to screen, saves to .mat file
%                     2 - saves data to .mat file and graphs to .png files
%                     (prints results to screen using a customized results file when there is one)

estim_options('max_step_number',1e4); % set options for parameter estimation
estim_options('max_fun_evals',5e4);  % set options for parameter estimation

estim_options('pars_init_method', 2);
estim_options('results_output', 1);
estim_options('method', 'nm');
estim_pars; % run estimation

load('results_Tiliqua_rugosa')
[stat,txt_stat]=statistics_std(par,chem,293,293,1,'std');
results=vertcat(metapar.T_ref,par.T_A,par.T_L,par.T_H,par.T_AL,par.T_AH,par.f,par.z,par.del_M,par.F_m,par.kap_X,par.kap_P,par.v,par.kap,par.kap_R,par.p_M,par.p_T,par.k_J,par.E_G,par.E_Hb,par.E_Hp,par.h_a,par.s_G,stat.E_0,stat.L_b,stat.L_i);
csvwrite('DEB_pars_Tiliqua_rugosa.csv',results);