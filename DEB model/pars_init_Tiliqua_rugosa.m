%% pars_init_my_pet
% sets (initial values for) parameters

%%
function [par, metaPar, txtPar] = pars_init_Tiliqua_rugosa(metaData)
metaPar.model = 'std'; % see online manual for explanation and alternatives

% reference parameter (not to be changed)
par.T_ref = C2K(20); free.T_ref = 0; units.T_ref = 'K';        label.T_ref = 'Reference temperature';

%% primary parameters
par.z = 7.262;        free.z     = 1;    units.z = '-';          label.z = 'zoom factor';
par.F_m = 6.5;        free.F_m   = 0;    units.F_m = 'l/d.cm^2'; label.F_m = '{F_M}, max spec searching rate';
par.kap_X = 0.85;     free.kap_X = 0;    units.kap_X = '-';      label.kap_X = 'digestion efficiency of food to reserve';
par.kap_P = 0.1;      free.kap_P = 0;    units.kap_P = '-';      label.kap_P = 'faecation efficiency of food to faeces';
par.v = 0.04796;       free.v     = 1;    units.v = 'cm/d';       label.v = 'energy conductance';
par.kap = 0.8119;        free.kap   = 1;    units.kap = '-';        label.kap = 'allocation fraction to soma';
par.kap_R = 0.95;     free.kap_R = 0;    units.kap_R = '-';      label.kap_R = 'reproduction efficiency';
par.p_M = 45.15;       free.p_M   = 1;    units.p_M = 'J/d.cm^3'; label.p_M = '[p_M], vol-spec somatic maint';
par.p_T =  0;         free.p_T   = 0;    units.p_T = 'J/d.cm^2'; label.p_T = '{p_T}, surf-spec somatic maint';
par.k_J = 0.002;      free.k_J   = 0;    units.k_J = '1/d';      label.k_J = 'maturity maint rate coefficient';
par.E_G = 7814;       free.E_G   = 1;    units.E_G = 'J/cm^3';   label.E_G = '[E_G], spec cost for structure';
par.E_Hb = 1.008e+05; free.E_Hb  = 1;    units.E_Hb = 'J';       label.E_Hb = 'maturity at birth';
par.E_Hp = 3.311e+05; free.E_Hp  = 1;    units.E_Hp = 'J';       label.E_Hp = 'maturity at puberty';
par.h_a = 3.135e-11;  free.h_a   = 1;    units.h_a = '1/d^2';    label.h_a = 'Weibull aging acceleration';
par.s_G = 1e-4;       free.s_G   = 0;    units.s_G = '-';        label.s_G = 'Gompertz stress coefficient';

%% auxiliary parameters
par.T_A   = 9625;     free.T_A   = 0;    units.T_A = 'K';        label.T_A = 'Arrhenius temperature';
par.T_AL   = 2e+04;   free.T_AL  = 0;    units.T_AL = 'K';       label.T_AL = 'low temp boundary';
par.T_AH   = 2e+04;   free.T_AH  = 0;    units.T_AH = 'K';       label.T_AH = 'high temp boundary';
par.T_L   = C2K(8.5); free.T_L   = 0;    units.T_L = 'K';        label.T_L = 'low Arrhenius temperature';
par.T_H   = C2K(39);  free.T_H   = 0;    units.T_H = 'K';        label.T_H = 'high Arrhenius temperature';
par.del_M = 0.2196;   free.del_M = 1;    units.del_M = '-';      label.del_M = 'shape coefficient';

%% Environmental parameters (temperatures are in data)
par.f = 1.0;          free.f     = 0;    units.f = '-';          label.f = 'scaled functional response for 0-var data';

%% set chemical parameters from Kooy2010 
[par, units, label,  free] = addchem(par, units, label, free, metaData.phylum, metaData.class);

%% Pack output:
txtPar.units = units; txtPar.label = label; par.free = free;
