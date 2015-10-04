%% mydata_my_pet
% Sets referenced data

%%
function [data, txt_data, metadata] = mydata_Tiliqua_rugosa 
  % created by Michael Kearney 2015/04/22
  
  %% Syntax
  % [data, txt_data, metadata] = <../mydata_my_pet.m *mydata_my_pet*>
  
  %% Description
  % Sets data, pseudodata, metadata, explanatory text, weight coefficients.
  % Meant to be a template in add_my_pet
  %
  % Output
  %
  % * data: structure with data
  % * txt_data: text vector for the presentation of results
  % * metadata: structure with info about this entry
  
%% set metadata

T_C = 273.15; % K, temperature at 0 degrees C (used in T_typical)

metadata.phylum     = 'Chordata'; 
metadata.class      = 'Reptilia'; 
metadata.order      = 'Squamata'; 
metadata.family     = 'Scincidae';
metadata.species    = 'Tiliqua_rugosa'; 
metadata.species_en = 'Sleepy Lizard'; 
metadata.T_typical  = T_C + 25; % K
metadata.data_0     = {'ab'; 'ap'; 'am'; 'Lb'; 'Lp'; 'Li'; 'Wdb'; 'Wdp'; 'Wdi'; 'Ri'};  % tags for different types of zero-variate data
metadata.data_1     = {'L-W','L-t','O-T'}; % tags for different types of uni-variate data

metadata.COMPLETE = 2.5; % using criteria of LikaKear2011

metadata.author   = {'Michael Kearney'};                              % put names as authors as separate strings:  {'author1','author2'} , with corresponding author in first place 
metadata.date_acc = [2015 04 24];                             % [year month day], date of entry is accepted into collection
metadata.email    = {'mrke@unimelb.edu.au'};                   % e-mail of corresponding author
metadata.address  = {'School of BioSciences, The University of Melbourne, 3010, Australia'};        % affiliation, postcode, country of the corresponding author

% uncomment and fill in the following fields when the entry is updated:
% metadata.author_mod_1  = {'author2'};                       % put names as authors as separate strings:  {'author1','author2'} , with corresponding author in first place 
% metadata.date_mod_1    = [2017 09 18];                      % [year month day], date modified entry is accepted into the collection
% metadata.email_mod_1   = {'myname@myuniv.univ'};            % e-mail of corresponding author
% metadata.address_mod_1 = {'affiliation, zipcode, country'}; % affiliation, postcode, country of the corresponding author

%% set data
% zero-variate data;
% typically depend on scaled functional response f.
% here assumed to be equal for all real data; the value of f is specified in pars_init_my_pet.

% age 0 is at onset of embryo development
data.ab = 30*5.;      units.ab = 'd';    label.ab = 'age at birth';                bibkey.ab = 'Kear2015';    
  temp.ab = T_C + 29.1;  % K, temperature, unused - predict file works it out from Munns data on temperature cycles
  % observed age at birth is frequently larger than ab, because of diapauzes during incubation
data.ap = data.ab+365*3-data.ab-365*1.5;     units.ap = 'd';    label.ap = 'age at puberty';              bibkey.ap = 'Kear2015';% 1 reproduced at 3 years old, but subtract away ab for gestation time of her baby plus a year for accumulation of repro buffer (downweighted in estimation proceedure)
  temp.ap = T_C + 29.1;  % K, temperature, unused - predict file works it out from Munns data on temperature cycles
  % observed age at puberty is frequently larger than ap, 
  %   because allocation to reproduction starts before first eggs appear
data.am = 50*365;     units.am = 'd';    label.am = 'life span';                   bibkey.am = {'Snid2002'}; % 21.9 in Snider 2002, probably an underestimate - have made it 50 years   
  temp.am = T_C + 23.2;  % K, temperature 
% (accounting for aging only) 

% Please specify what type of length measurement is used for your species.
% We put here snout-to-vent length, but you should change this depending on your species:
data.Lb  = 16.5;   units.Lb  = 'cm';   label.Lb  = 'snout to vent length at birth';    bibkey.Lb  = 'Kear2015'; % Munns data (n=13)
data.Lp  = 23.;   units.Lp  = 'cm';   label.Lp  = 'snout to vent length at puberty';  bibkey.Lp  = 'Kear2015'; % from Munns observations of length at age
data.Li  = 32.8;   units.Li  = 'cm';   label.Li  = 'ultimate snout to vent length';    bibkey.Li  = 'Kear2015'; % average length of Munns animals (females) 2,4,17,20,4,6,10,17,18
data.Wdb = 99*0.3; units.Wdb = 'g';    label.Wdb = 'dry weight at birth';              bibkey.Wdb = 'Kear2015'; % Munns data (n=15)
data.Wdp = 180*0.3;   units.Wdp = 'g';    label.Wdp = 'dry weight at puberty';            bibkey.Wdp = 'Kear2015'; % from Munns observations of length vs. weight, using Lp above
data.Wdi = 717*0.3;   units.Wdi = 'g';    label.Wdi = 'ultimate dry weight';              bibkey.Wdi = 'Kear2015'; % average post partum mass of Munns animals 2,4,17,20,4,6,10,17,18
data.Ri  = (1.5+(0.17*1.5))/(2*365);    units.Ri  = '#/d';  label.Ri  = 'maximum reprod rate';              bibkey.Ri  = 'Pamu1997';   % maximum reproduction rate at f (for individual of max length), Munns data suggests 1.5 per 2 years 
  % for an individual of ultimate length Li 
  temp.Ri = T_C +  23.2;  % K, temperature, unused - predict file works it out from Munns data on temperature cycles
 
% uni-variate data

data.TO = [8.2246	9.0217	9.0942	9.0942	9.6377	9.7101	14.6739	14.7464	14.7826	14.8551	15.0362	15.2536	15.3261	19.4565	19.6377	19.6377	19.6739	19.7101	19.9638	20	20	20.1812	20.3261	20.3623	24.6377	24.6739	24.7464	25	25.0725	25.0725	25.1087	25.1087	25.1449	29.4565	29.6014	29.7101	29.7101	29.8551	29.9638	30	30	30.0725	30.2174	30.2536	30.2536	30.3623	30.5796	34.6739	34.8551	34.8551	35.0725	35.1812	35.2536	35.3623	35.3623	35.4348	35.6522	35.8696	36.3768;	%39.3841	39.529	39.7826	39.8551	40	40.0362	40.1812	40.8333	41.0507	41.4493;      % ml/h/g, O2 consumption  of 138.3 g dry weight (461 g wet)
0.00499	0.00605	0.005	0.00803	0.00806	0.00604	0.0131	0.0221	0.0152	0.0202	0.0202	0.0182	0.0132	0.0244	0.0375	0.0426	0.0335	0.0355	0.0386	0.0203	0.0376	0.0285	0.0335	0.0405	0.05	0.0748	0.056	0.085	0.0438	0.0836	0.0492	0.0748	0.0574	0.0914	0.1314	0.0836	0.0852	0.074	0.0965	0.0636	0.0813	0.1049	0.0911	0.0846	0.0958	0.1022	0.0841	0.1511	0.1476	0.083	0.1163	0.1965	0.1273	0.1623	0.1069	0.1707	0.1095	0.1292	0.1305]%	%0.1429	0.1597	0.1498	0.1669	0.1484	0.1608	0.1935	0.233	0.2307	0.1958];
data.TO=transpose(data.TO)
units.TO = {'C','mlO2/gwet/h'};     label.TO = {'temperature','O2 consumption rate'};  bibkey.TO = 'Wils1974';

data.tW = [127.75	127.75	127.75	127.75	127.75	127.75	127.75	127.75	127.75	128.75	128.75	132.75	132.75	133.75	133.75	133.75	133.75	134.75	134.75	134.75	135.75	135.75	140.75	140.75	140.75	140.75	141.75	141.75	142.75	142.75	147.75	147.75	147.75	147.75	148.75	148.75	149.75	149.75	154.75	154.75	154.75	154.75	155.75	155.75	155.75	156.75	156.75	161.75	161.75	161.75	161.75	162.75	162.75	163.75	163.75	168.75	168.75	168.75	168.75	169.75	169.75	170.75	170.75	175.75	175.75	175.75	175.75	176.75	176.75	177.75	177.75	177.75	182.75	182.75	182.75	182.75	183.75	183.75	184.75	184.75	187.75	189.75	189.75	189.75	190.75	190.75	192.75	197.75	198.75	206.75	209.75	209.75	213.75	214.75	214.75	214.75	215.75	215.75	220.75	220.75	221.75	222.75	225.75	225.75	225.75	226.75	226.75	227.75	227.75	227.75	232.75	232.75	232.75	233.75	233.75	238.75	250.75	256.75	261.75	268.75	268.75	273.75	273.75	273.75	274.75	274.75	274.75	293.75	293.75	298.75	298.75	298.75	299.75	299.75	300.75	302.75	303.75	303.75	308.75	308.75	308.75	308.75	309.75	309.75	310.75	310.75	315.75	315.75	315.75	315.75	316.75	316.75	317.75	317.75	317.75	322.75	322.75	322.75	322.75	323.75	323.75	324.75	324.75	329.75	329.75	329.75	330.75	330.75	331.75	331.75	336.75	336.75	336.75	336.75	337.75	337.75	338.75	343.75	353.75	355.75	363.75	369.75	372.75	376.75	377.75	377.75	379.75	382.75	382.75	382.75	383.75	383.75	386.75	386.75	391.75	391.75	391.75	392.75	392.75	393.75	393.75	398.75	398.75	398.75	399.75	399.75	400.75	400.75	400.75	405.75	405.75	405.75	406.75	406.75	407.75	407.75	412.75	412.75	412.75	413.75	413.75	414.75	414.75	419.75	419.75	419.75	420.75	420.75	423.75	423.75	428.75	428.75	428.75	428.75	428.75	428.75	428.75	429.75	429.75	433.75	433.75	433.75	434.75	434.75	435.75	435.75	440.75	440.75	440.75	441.75	441.75	442.75	446.75	456.75	459.75	466.75	470.75	473.75	483.75	484.75	491.75	497.75	514.75	525.75	533.75	539.75	553.75	576.75	599.75	604.75	610.75	617.75	624.75	634.75	634.75	640.75	648.75	655.75	662.75	666.75	672.75	676.75	690.75	704.75	708.75	712.75	725.75	731.75	737.75	747.75	758.75	773.75	799.75	814.75	825.75	829.75	842.75	845.75	865.75	883.75	894.75	896.75	896.75	917.75	940.75	946.75	985.75	1034.75	1044.75	1064.75	1078.75	1085.75	1087.75	1092.75	1099.75	1104.75	1106.75	1113.75	1120.75	1127.75	1129.75	1144.75	1155.75	1160.75	1162.75	1169.75	1176.75	1183.75	1188.75	1190.75	1197.75	1204.75	1205.75	1205.75	1210.75	1211.75	1214.75	1218.75	1222.75	1225.75	1232.75	1236.75	1239.75	1246.75	1249.75	1253.75	1255.75	1260.75	1264.75	1267.75	1271.75	1278.75	1285.75	1292.75	1292.75	1299.75	1303.75	1306.75	1310.75	1313.75	1351.75	1369.75	1376.75	1386.75	1393.75	1400.75	1407.75	1414.75	1431.75	1439.75	1446.75	1453.75	1460.75	1467.75	1469.75	1474.75	1476.75	1483.75	1486.75	1490.75	1494.75	1497.75	1500.75	1507.75	1511.75	1518.75	1559.75	1577.75	1590.75	1597.75	1604.75	1614.75	1622.75	1664.75	1730.75	1741.75	1748.75	1755.75	1765.75	1771.75	1779.75	1786.75	1793.75	1803.75	1807.75	1821.75	1835.75	1843.75	1856.75	1868.75	1878.75	1889.75	1956.75	1976.75	2014.75	2027.75	2028.75	2048.75	2077.75	2175.75	2195.75	2209.75	2216.75	2223.75	2230.75	2237.75	2244.75	2251.75	2258.75	2286.75	2293.75	2300.75	2307.75	2314.75	2321.75	2328.75	2335.75	2336.75	2342.75	2349.75	2363.75	2370.75	2377.75	2384.75	2391.75	2398.75	2423.75	2434.75	2441.75	2482.75	2507.75	2517.75	2524.75	2531.75	2538.75	2545.75	2591.75	2600.75	2607.75	2614.75	2621.75	2637.75	2642.75	2649.75; ...
78	90.5	102	112	115	89	102	104	85	103	68	100	100	92	91	80	91	92	117	119	96	78	95	94	84	94	121	129	93	80	105	105	86	105	131	143	102	92	118	103	89	103	125	135	149	126	99	118	114	98	114	149	162	130	110	115	123	103	123	157	172	135	107	113	132	104	132	160	179	158	151	137	119	164	147	164	163	241	162	157	178	174	154	174	174	250	199	111	203	213	161	163	229	184	177	184	195	261	158	160	240	152	182	167	182	188	249	259	156	156	180	162	180	190	239	167	275	166	260	185	180	205	184	205	272	212	315	203	207	230	200	230	222	303	194	242	223	207	211	217	205	217	227	309	236	241	207	259	232	259	264	343	291	241	256	213	259	232	259	260	353	252	258	272	254	272	273	355	265	274	248	290	267	290	294	376	292	244	257	225	237	230	324	242	307	324	333	319	296	319	319	396	316	331	374	322	374	323	461	321	367	369	331	369	334	440	341	316	375	397	318	397	380	472	324	346	397	317	397	350	459	310	398	399	330	399	378	430	330	399	406	263	440	343	440	334	417	412	481	427	340	427	373	483	337	446	430	341	430	432	502	486	356	440	416	465	470	427	441	470	444	483	466	512	460	520	569	578	435	572	445	445	436	590	434	448	473	460	453	537	463	498	491	510	548	531	555	569	610	617	620	629	674	684	605	673	687	598	696	609	725	593	626	648	553	606	620	619	608	567	695	707	624	676	733	606	703	729	765	788	551	618	785	699	806	824	813	818	670	821	822	819	699	814	605	604	700	679	675	666	703	678	717	701	707	699	741	675	685	656	743	743	756	763	658	715	649	700	646	696	630	680	675	752	695	689	690	736	663	647	673	656	741	653	730	721	759	785	703	805	720	726	714	641	708	728	725	714	716	702	698	815	685	693	665	668	660	663	636	623	644	639	637	616	637	631	630	691	712	729	739	748	724	738	848	765	794	798	782	720	704	794	803	815	789	781	746	757	765	827	869	853	866	847	854	864	858	834	850	826	774	811	802	816	828	835	819	814	811	794	804	890	822	803	799	833	869	824	847	861	831	867	882	897];
data.tW=transpose(data.tW);
units.tW = {'d', 'g'};     label.tW = {'age', 'weight'};  bibkey.TO = 'Kear2015';

data.tL = [128.75	134.75	127.75	133.75	127.75	133.75	129.75	128.75	128.75	134.75	134.75	133.75	134.75	829.75	830.75	825.75	829.75	447.75	834.75	447.75	446.75	446.75	1215.75	1255.75	447.75	441.75	440.75	1216.75	1956.75	447.75	2342.75	1211.75	1220.75	2655.75	1215.75	1528.75	1529.75	1533.75	1524.75	1528.75	1956.75	1255.75	2342.75	2655.75; ...
14.6	15	15	15.3	15.7	16	16.4	16.7	16.8	17	17.5	18.7	20	22	23.4	24.1	24.8	24.8	25.2	25.2	25.5	26.1	26.3	26.8	26.8	26.8	26.8	27	27.1	27.3	27.5	28	28.3	28.4	29	29.5	30.3	31	32.4	32.8	34	34.4	37	38.9];
data.tL=transpose(data.tL);
units.tL = {'d', 'cm'};     label.tL = {'age', 'length'};  bibkey.TO = 'Kear2015';

data.LW = [14.6	15	15	15.3	15.7	16	16.4	16.7	16.8	17	17.5	18.7	20	24.8	25.2	25.5	26.1	26.8	26.8	26.8	26.8	27.1	27.3	27.5	27.5	28.4	28.5	29	29	29	29	29.5	29.5	29.7	30	30	30	30.2	30.3	30.5	30.5	30.5	30.7	30.9	31	31	31	31	31.1	31.2	31.5	31.5	31.5	31.5	31.6	31.7	32	32	32	32.2	32.2	32.4	32.5	32.5	32.5	32.5	32.5	32.7	32.7	32.8	32.8	33.1	33.1	33.1	33.1	33.5	33.5	33.5	33.5	33.5	33.6	33.6	33.7	33.7	33.9	34	34	34	34	34	34	34	34.2	34.4	34.4	34.4	34.6	34.6	34.7	34.8	35	35	35.3	35.5	37	38.9; ... % length (cm), weight (g)
82	85	68	80	90.5	91	94	103	102	117	119	132	127	432	391	341	430	491	459	337	446	481	502	511	486	534	643	668	709	630	689	494	611	662	657	690	542	884	594	658	642	680	1056	1036	655	812	891	724	674	826	757	845	987	918	668	697	772	1024	821	941	916	728	717	614	951	861	893	994	1037	718	1020	740	1059	698	1076	798	916	1072	960	760	881	804	950	827	1061	724	847	858	1079	828	1079	784	844	741	1034	973	697	885	1051	1145	1168	970	665	755	850	897];
data.LW = transpose(data.LW);
units.LW = {'cm', 'g'};     label.LW = {'snout vent length', 'wet weight'};  bibkey.LW = 'Kear2015';

%% set weights for all real data
weight = setweights(data, []);

%% overwriting weights (remove these remarks after editing the file)
% the weights were set automatically with the function setweigths,
% if one wants to ovewrite one of the weights it should always present an explanation example:
%
% zero-variate data:
% weight.Wdi = 100 * weight.Wdi; % Much more confidence in the ultimate dry
%                                % weight than the other data points
weight.Wdb = weight.Wdb*10;
weight.Wdi = weight.Wdi*20;
weight.Lb = weight.Lb*10;
weight.Li = weight.Li*10;
weight.ab = weight.ab*10;
weight.Ri = weight.Ri*1000;

% % uni-variate data: 
  weight.tW = 100 * weight.tW;

%% set pseudodata and respective weights
% (pseudo data are in data.psd and weights are in weight.psd)
[data, units, label, weight] = addpseudodata(data, units, label, weight);

%% overwriting pseudodata and respective weights (remove these remarks after editing the file)
% the pseudodata and respective weights were set automatically with the function setpseudodata
% if one wants to ovewrite one of the values it should always present an explanation
% example:
% data.psd.p_M = 1000;                    % my_pet belongs to a group with high somatic maint 
% weight.psd.kap = 10 * weight.psd.kap;   % I need to give this pseudo data a higher weight

%% pack data and txt_data for output
data.weight = weight;
data.temp = temp;
txt_data.units = units;
txt_data.label = label;
txt_data.bibkey = bibkey;

%% References
  bibkey = 'Wiki'; type = 'Misc'; bib = ...
  'URL = {http://en.wikipedia.org/wiki/Tiliqua_rugosa}';   % replace my_pet by latin species name
  eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
  %
  bibkey = 'Kooy2010'; type = 'Book'; bib = [ ...  % used in setting of chemical parameters and pseudodata
  'author = {Kooijman, S.A.L.M.}, ' ...
  'year = {2010}, ' ...
  'title  = {Dynamic Energy Budget theory for metabolic organisation}, ' ...
  'publisher = {Cambridge Univ. Press, Cambridge}, ' ...
  'pages = {Table 4.2 (page 150), 8.1 (page 300)}, ' ...
  'URL = {http://www.bio.vu.nl/thb/research/bib/Kooy2010.html}'];
  eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
  %
  bibkey = 'Kear2015'; type = 'Article'; bib = [ ...  % 
   'author = {Kearney, M. R. and Munns, Suzanne L. and Malishev, M. and Bull, C. M.}, ' ...
   'title = {Dissecting the relative roles of water and temperature in constraining ectotherm activity}, ' ...
   'journal = {??}, ' ...
   'year = {in prep}, ' ...
   'type = {Journal Article}'];
  eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
  %
  bibkey = 'Pamu1997'; type = 'Thesis'; bib = [ ... % meant as example; replace this and further bib entries
   'author = {Pamula, Y}, ' ...
   'title = {Reproduction in the viviparous skink Tiliqua rugosa}, ' ...
   'university = {Flinders University}, ' ...
   'year = {1997}, ' ...
   'type = {Thesis}'];
  eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
  %
  bibkey = 'Kear2004c'; type = 'Article'; bib = [ ...  % 
   'author = {Wilson, K. J.}, ' ...
   'title = {The Relationship of Oxygen Supply for Activity to Body Temperature in Four Species of Lizards}, ' ...
   'journal = {Copeia}, ' ...
   'volume = {1974}, ' ...
   'number = {4}, ' ...
   'pages = {920-934}, ' ...
   'ISSN = {00458511}, ' ...
   'DOI = {10.2307/1442592}, ' ...
   'url = {http://www.jstor.org/stable/1442592}, ' ...
   'year = {1974}, ' ...
   'type = {Journal Article}'];
  eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
 
%% Facts
% * Standard model with egg (not foetal) development and no acceleration
  
%% Discussion points
pt1 = 'Author_mod_1: The T_A is estimated from data on O2 consumption at different temperatures and weight with time at known fluctuating temperatures';
pt2 = 'Author_mod_1: Known fluctuating temperatures are converted to a mean TC within the predict file which changes as the estimated T_A changes';

metadata.discussion = {pt1}; 
metadata.discussion = {pt2}; 

