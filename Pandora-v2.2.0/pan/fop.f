      subroutine FOP
     $(NO)
C
C     Rudolf Loeser, 1976 Apr 05
C     Revised RL/SGK Jul 14 2014 
C---- Processes OPTION switches, and prints their final states.
C     !DASH
      save
C     !DASH
      integer I, IOPPNT, IQOPP, J, K, KT, LU, NCURR, NO, jummy
      character LEGS*109, MLAB*4, NULL*4, OSCAR*20, qummy*8
C     !COM
C---- OPTIONS     as of Jul 14 2014 (RL/SGK)
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=346)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(228),IQOPP)
C
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C     !DASH
      external WRENCH, JIGGLE, LINER, ALYSSUM, PRIAM, MALACH, MUCKLE,
     $         BARK, ZEUS, HI, BYE
C
      dimension IOPPNT(NOOPT), LEGS(NOOPT), OSCAR(NOOPT)
C
      data NULL /'0000'/
C     !EJECT
      data LEGS(  1),LEGS(  2),LEGS(  3),LEGS(  4),LEGS(  5) /
     $'Print detailed analysis of Line Absorption Profiles (Hydrogen is -
     $special!), (parameters NANAL1, NANAL2).',
     $'Print collated Tau scales.',
     $'Print details of Statistical Equilibrium calculations for transit-
     $ion (MS/NS).',
     $'Print TAU, FR and S for LTE line and flux calculations.',
     $'Spectrum calculations, and spectrum summary analyses.'/
C
      data LEGS(  6),LEGS(  7),LEGS(  8),LEGS(  9),LEGS( 10) /
     $'Calculate Continuum Eclipse Intensity (for negative "Additional W-
     $avelengths" only).',
     $'Print comparison of the results obtained from the various methods-
     $ for the Lyman EP1, EP2 calculations.',
     $'Use spherical coordinates to calculate Line and Continuum fluxes;-
     $ (will be switched ON if SPHERE=ON).',
     $'Calculate Line spectra.',
     $'Save transition data in special file (.tsf).'/
C
      data LEGS( 11),LEGS( 12),LEGS( 13),LEGS( 14),LEGS( 15) /
     $'Print results of continuum calculations for Additional Wavelength-
     $s.',
     $'Print detailed analysis of continuum opacity contributions.',
     $'Calculate Level-N-to-Continuum ("Lyman") transfer.',
     $'Use level-1 TR in calculation of Stimulated Emission factors for -
     $BC; (if =OFF then use TE).',
     $'Print set of "transition terms" (also needs values of LDINT and L-
     $DTYP).'/
C
      data LEGS( 16),LEGS( 17),LEGS( 18),LEGS( 19),LEGS( 20) /
     $'Calculate hydrostatic equilibrium; (will be switched OFF if SPHER-
     $E=ON).',
     $'Print timings for the different statistical equlibrium methods.',
     $'Print Iteration and Fudging Summaries.',
     $'Print all enabled output for every subiteration; (if =OFF then pr-
     $int this for the last subiteration only).',
     $'Print Kurucz Statistical Opacity data (note input parameter LWNT)-
     $.'/
C
      data LEGS( 21),LEGS( 22),LEGS( 23),LEGS( 24),LEGS( 25) /
     $'Print provisional input (i.e. before interpolation to Z-table of -
     $this run).',
     $'Print debug details of fast electron calculation: injection funct-
     $ions.',
     $'Print Stimulated Emission factors.',
     $'Print results of Rates Calculations (for minimal printout instead-
     $, use input parameter IRATE).',
     $'Print values of Damping Parameter, and of Doppler Width.'/
C
      data LEGS( 26),LEGS( 27),LEGS( 28),LEGS( 29),LEGS( 30) /
     $'Print details of Line Center TAU calculation.',
     $'Print Carbon populations data.',
     $'Print complete sets of BDR, BDJ, BDS, and S* from RHO+BD calculti-
     $on (only if RHBPRNT=ON).',
     $'Print Populations of the ion of this run.',
     $'Print debug details of PRD DR calculation for transition (MS/NS) -
     $at depth IDRDP and frequency KDRDP.'/
C     !EJECT
      data LEGS( 31),LEGS( 32),LEGS( 33),LEGS( 34),LEGS( 35) /
     $'Use spherical coordinates for source function calculations.',
     $'Set VT equal to V.',
     $'Calculate LTE line intensity and flux profiles.',
     $'Print graph of Populations of the ion of this run.',
     $'Adjust recomputed total hydrogen density so that TAU5000 = 1 at Z-
     $ = 0, (used only if HSE=ON).'/
C
      data LEGS( 36),LEGS( 37),LEGS( 38),LEGS( 39),LEGS( 40) /
     $'Print Hydrogen populations data.',
     $'Print results of continuum calculations for Additional Photoioniz-
     $ation wavelengths.',
     $'Use R**2 enhancement factor in the calculation of emergent intens-
     $ities; (will be switched OFF if SPHERE=ON).',
     $'Print debug details for Continuum Source Function calculation.',
     $'Print additional details of Line Eclipse calculation.'/
C
      data LEGS( 41),LEGS( 42),LEGS( 43),LEGS( 44),LEGS( 45) /
     $'Save insurance Restart Data after the end of every overall iterat-
     $ion.',
     $'Print results of Passive Lines source function calculation.',
     $'Print intermediate results of the emergent line profile calculati-
     $ons.',
     $'Print graphs of emergent line profiles, and of S vs. Z.',
     $'Print results of Continuum Source Function calculation.'/
C
      data LEGS( 46),LEGS( 47),LEGS( 48),LEGS( 49),LEGS( 50) /
     $'Use computed Continuum Source Function when calculating Line Sour-
     $ce function.',
     $'Print graphs of CHECKs for all iterations.',
     $'Print Helium-I populations data.',
     $'In this calculation the medium is of finite extent.',
     $'The finite medium is symmetric about the last specified depth, (u-
     $sed only if FINITE=ON).'/
C
      data LEGS( 51),LEGS( 52),LEGS( 53),LEGS( 54),LEGS( 55) /
     $'The external radiation field is incident upon the back face of th-
     $e finite medium; (unless INCIFRONT=ON).',
     $'Print results of continuum calculations for Rates Integrations wa-
     $velengths.',
     $'Use the specified input values of TR to calculate RK and RL.',
     $'Force the calculated TAU values to increase monotonically.',
     $'Calculate emergent continuous intensities.'/
C
      data LEGS( 56),LEGS( 57),LEGS( 58),LEGS( 59),LEGS( 60) /
     $'Compute NE in detail, (if OFF then set NE = NP).',
     $'Print additional details of Line Profile calculations, for transi-
     $tion (MS/NS).',
     $'Print additional details of Emergent Continuous Intensity calcula-
     $tion.',
     $'Print ABSORBERS Summary.',
     $'Use geometrical dilution terms; (will be switched OFF if SPHERE=O-
     $N).'/
C     !EJECT
      data LEGS( 61),LEGS( 62),LEGS( 63),LEGS( 64),LEGS( 65) /
     $'Calculate Incident Line Radiation term, (used only if INCIDNT=ON)-
     $.',
     $'Let the Dilution Factor vary with depth.',
     $'Print graph of contributions to continuous opacity.',
     $'Print details of TAU integrations.',
     $'Print results of continuum calculations for core wavelengths of l-
     $ines with LSF or profile printout.'/
C
      data LEGS( 66),LEGS( 67),LEGS( 68),LEGS( 69),LEGS( 70) /
     $'Print detailed analysis of continuum emission contributions.',
     $'Print details and graphs of results of Electron Density calculati-
     $on.',
     $'Calculate a non-LTE H- source function.',
     $'Print results of continuum calculations for wavelengths used for -
     $the non-LTE H- source function calculation.',
     $'Print results of Partial Redistribution calculations.'/
C
      data LEGS( 71),LEGS( 72),LEGS( 73),LEGS( 74),LEGS( 75) /
     $'Edit negative values out of EP1, by interpolation using neighbori-
     $ng values.',
     $'Edit negative values out of EP1, by shifting the whole set of val-
     $ues.',
     $'Print results of continuum calculations for wavelengths used for -
     $PRD calculations (uses IPRDF).',
     $'Print Silicon populations data.',
     $'Print details of Statistical Equilibrium calculations.'/
C
      data LEGS( 76),LEGS( 77),LEGS( 78),LEGS( 79),LEGS( 80) /
     $'Print various basic numerical data.',
     $'Use standard rates integrations wavelengths table.',
     $'Edit negative values out of initial values of BD-ratios, (used on-
     $ly if RHOFUDGE=OFF).',
     $'Edit negative values out of integrands used to compute TAU.',
     $'Calculate Continuum Flux.'/
C
      data LEGS( 81),LEGS( 82),LEGS( 83),LEGS( 84),LEGS( 85) /
     $'Print results of fast electrons calculation.',
     $'Print standard rates integrations wavelengths table.',
     $'Smooth out the computed "Lyman" RK1 values.',
     $'Calculate Color Temperatures.',
     $'Print details of generalized additional photoionization calculati-
     $on.'/
C
      data LEGS( 86),LEGS( 87),LEGS( 88),LEGS( 89),LEGS( 90) /
     $'Print details of geometrical dilution term calculation, (used onl-
     $y if GDS=ON).',
     $'Print input values of Number Density and Departure Coefficient of-
     $ the ion of this run.',
     $'Print input values of RHO, JBAR, CHI and AW.',
     $'Print graph of logs of TAU scales.',
     $'Print Helium-II populations data.'/
C     !EJECT
      data LEGS( 91),LEGS( 92),LEGS( 93),LEGS( 94),LEGS( 95) /
     $'Print Aluminum populations data.',
     $'Print graph of contributions to continuous emission.',
     $'Print summary graph of continuum calculation results, and graph o-
     $f continuous opacity.',
     $'Print all enabled output for every overall iteration; (if =OFF th-
     $en print for last overall iteration only).',
     $'Print graph of TR, and graph of Jnu, obtained from the Rates Inte-
     $grations.'/
C
      data LEGS( 96),LEGS( 97),LEGS( 98),LEGS( 99),LEGS(100) /
     $'Print Spectrum Summary.',
     $'Save Continuous Spectrum data in the Special Spectrum Save file (-
     $will be turned ON when SPECSAV=ON).',
     $'Print Magnesium populations data.',
     $'Use Type-2 method to calculate dust opacity.',
     $'Print results of continuum calculations for wavelengths used in T-
     $ype-2 dust opacity calculation.'/
C
      data LEGS(101),LEGS(102),LEGS(103),LEGS(104),LEGS(105) /
     $'Print Depths-of-Formation Summary.',
     $'Print details of Continuum Flux calculation, as controlled by the-
     $ input parameter IFXDS.',
     $'Print comparison of results obtained from the various methods for-
     $ the statistical equlibrium calculations.',
     $'Print EMITTERS Summary.',
     $'Print TAU Summary.'/
C
      data LEGS(106),LEGS(107),LEGS(108),LEGS(109),LEGS(110) /
     $'Fudge RHO values as necessary, to obtain acceptable values from t-
     $he BD-ratio calculation.',
     $'Print all enabled Lyman/HSE output for every iteration; (if =OFF -
     $then print this for last iteration only).',
     $'Print results of Rates Integrations in full detail, (used only if-
     $ RATEPRNT=ON).',
     $'Print iterative summaries of line source functions.',
     $'Print iterative summaries of net radiative brackets.'/
C
      data LEGS(111),LEGS(112),LEGS(113),LEGS(114),LEGS(115) /
     $'Print iterative summaries of line center optical depths.',
     $'Print iterative summary of "Lyman" RK1.',
     $'Print iterative summaries of number densities.',
     $'Print iterative summaries of RHO weights.',
     $'Print results of continuum calculations for Level-N-to-Continuum -
     $("Lyman") transfer calculation wavelengths.'/
C
      data LEGS(116),LEGS(117),LEGS(118),LEGS(119),LEGS(120) /
     $'Print details of Level-N-to-Continuum ("Lyman") transfer calculat-
     $ion.',
     $'Print execution performance statistics.',
     $'Print weighted number densities and departure coefficients.',
     $'Print canonical matrix of geometrical dilution terms, (used only -
     $if GDS=ON).',
     $'Print details of RHO+BD calculation for each radiative transition-
     $ (only if RHBPRNT=ON).'/
C     !EJECT
      data LEGS(121),LEGS(122),LEGS(123),LEGS(124),LEGS(125) /
     $'Print detailed results from "Lyman" EP1,EP2 calculation, (not use-
     $d if METEP=0).',
     $'Edit out negative values of "Lyman" EP1, when the "CHAIN" method -
     $(i.e. METEP=3) is used.',
     $'Use logarithmic style of weighting for RHO.',
     $'Use logarithmic style of weighting for "Lyman" EP1,EP2.',
     $'Print Sulfur populations data.'/
C
      data LEGS(126),LEGS(127),LEGS(128),LEGS(129),LEGS(130) /
     $'Print the RK Comparison.',
     $'Print iterative summary of Electron Density.',
     $'Print iterative summaries of Departure Coefficients.',
     $'Print dump of number density calculation.',
     $'Print debug details of Departure Coefficient calculation.'/
C
      data LEGS(131),LEGS(132),LEGS(133),LEGS(134),LEGS(135) /
     $'Print Iron populations data.',
     $'Print debug details for HSE calculation.',
     $'Calculate net cooling rates.',
     $'Save net cooling and heatings rates data in a special file.',
     $'The internal radiation travels in the outward direction only.'/
C
      data LEGS(136),LEGS(137),LEGS(138),LEGS(139),LEGS(140) /
     $'Print iterative summary of Z.',
     $'DIANA/ORION line source function calculation dump for transition -
     $(MS/NS): but no details, blocks.',
     $'DIANA/ORION line source function calculation dump for transition -
     $(MS/NS): summation details only.',
     $'Print details of H- and H free-free cooling rates calculation.',
     $'Calculate integrated net cooling and heating rates.'/
C
      data LEGS(141),LEGS(142),LEGS(143),LEGS(144),LEGS(145) /
     $'Radiation incident upon the front face of the medium, finite or s-
     $emi-infinite (used only if INCIDNT=ON).',
     $'Print debug summary of contents of Iterative Summary File (=IBIS)-
     $.',
     $'Print debug details of General Conductive Flux calculation.',
     $'Check validity of the integrations involved in the calculations o-
     $f emergent intensities.',
     $'Remove negatives from frequency-dependent line source functions u-
     $sed for intensity and flux profiles.'/
C
      data LEGS(146),LEGS(147),LEGS(148),LEGS(149),LEGS(150) /
     $'Print debug details of WN-Matrix calculation.',
     $'Do sequential smoothing of RHO values.',
     $'Print messages concerning sequential smoothing.',
     $'Edit calculated line source function.',
     $'Edit calculated net radiative bracket.'/
C     !EJECT
      data LEGS(151),LEGS(152),LEGS(153),LEGS(154),LEGS(155) /
     $'Save Emergent Line Profiles data in the Special Spectrum Save fil-
     $e (will be turned ON when SPECSAV=ON).',
     $'Interpolate input values of Jnu, (used only if TAUK is input).',
     $'Print dump of component profiles of blended lines.',
     $'Compute Jnu directly, rather than from source function.',
     $'Print details of Recombination calculation.'/
C
      data LEGS(156),LEGS(157),LEGS(158),LEGS(159),LEGS(160) /
     $'Compute TAU(ray) by quadratic integration, (if =OFF then use trap-
     $ezoidal rule); (used only if SPHERE=ON).',
     $'Print Z-dependent geometrical ray quantities, (used only if SPHER-
     $E=ON).',
     $'Set higher departure coefficients of population ions equal to hig-
     $hest computed non-LTE values.',
     $'DIANA/ORION line source function calculation dump for transition -
     $(MS/NS): data blocks contents only.',
     $'Save line eclipse emission data in the restart data file.'/
C
      data LEGS(161),LEGS(162),LEGS(163),LEGS(164),LEGS(165) /
     $'Edit calculated departure coefficients, to prevent negative CSF v-
     $alues.',
     $'Print graphs of line eclipse calculation results.',
     $'Include the ray tangent to each depth in angle integrations for s-
     $pherical coordinates, (used only if NTAN=1).',
     $'Print tables of Ionization Potential, and of Partition Functions -
     $and/or ratios.',
     $'Let Partition Functions vary with depth.'/
C
      data LEGS(166),LEGS(167),LEGS(168),LEGS(169),LEGS(170) /
     $'In line source function calculation: set BC = min(CSF, B), (if =O-
     $FF then BC = CSF); (used only if CSF=ON).',
     $'Print P.R.D. Jnu every time they are calculated.',
     $'Edit QSF (i.e. P.R.D. modified source function).',
     $'This run is for an expanding atmosphere.',
     $'Print Kurucz Composite Line Opacity data (note input parameter LW-
     $NT).'/
C
      data LEGS(171),LEGS(172),LEGS(173),LEGS(174),LEGS(175) /
     $'Print results of continuum calculations for Composite Line Opacit-
     $y wavelengths.',
     $'Apply frequency shift to P.R.D. Snu used in emergent line profile-
     $ calculation.',
     $'Use two broadening velocities, (if =OFF then use a single broaden-
     $ing velocity).',
     $'Print dump of contents of Random Access files indices.',
     $'Print results of particle energy dissipation calculation.'/
C
      data LEGS(176),LEGS(177),LEGS(178),LEGS(179),LEGS(180) /
     $'Print debug details of particle energy dissipation calculation.',
     $'Use results of fast electrons calculation.',
     $'Calculate rates due to fast electrons.',
     $'Print debug check-sums.',
     $'Print all integration data for rates calculation.'/
C     !EJECT
      data LEGS(181),LEGS(182),LEGS(183),LEGS(184),LEGS(185) /
     $'Print results of continuum calculations for K-Shell wavelengths.'
     $,
     $'Print mean intensities used in non-LTE H- calculations.',
     $'Use only continuum data from "H- wavelengths" in non-LTE H- calcu-
     $lations.',
     $'Adjust total Hydrogen density to give constant pressure, (used on-
     $ly if HSE=OFF).',
     $'Replace any Source Function Epsilons less than -0.9999 by -0.9999-
     $.'/
C
      data LEGS(186),LEGS(187),LEGS(188),LEGS(189),LEGS(190) /
     $'Try other Statistical Equilibrium methods whenever any Epsilon is-
     $ less than -0.9999.',
     $'Print details of Statistical Equilibrium method selection (used o-
     $nly if METSW=ON).',
     $'Print full Line Flux Distribution array, and other auxiliary arra-
     $ys.',
     $'Print RIJ = CIJ + PIJ.',
     $'Print short form of "ORIGINS" printout.'/
C
      data LEGS(191),LEGS(192),LEGS(193),LEGS(194),LEGS(195) /
     $'Save integrated flux quantities in restart data file.',
     $'Save spectrum data in special file; (this option =ON is equivalen-
     $t to CONSAV and PROSAV both =ON).',
     $'Print long version of error message when computed B-ratio is less-
     $ than zero.',
     $'Calculate Composite Lines cooling rate (only if only one band).',
     $'Print dump of Composite Lines cooling rates integration.'/
C
      data LEGS(196),LEGS(197),LEGS(198),LEGS(199),LEGS(200) /
     $'Calculate X-rays cooling rate.',
     $'Print dump of X-rays cooling rates integration.',
     $'Print dump of Rosseland-mean opacity integration.',
     $'Print results of RHO+BD calculation (see also options RHBPRDT, RH-
     $BPRSM, BDPRNT).',
     $'Print results of continuum calculations for frequency-dependent l-
     $ine background.'/
C
      data LEGS(201),LEGS(202),LEGS(203),LEGS(204),LEGS(205) /
     $'Print iterative summary of Total Hydrogen Density.',
     $'Print frequency-dependent background terms for FDB transitions.',
     $'Print dump of details of Sobolev escape probability solution for -
     $transition (MS,NS).',
     $'Use "Artificial TAU" in RHO/W calculation.',
     $'Print minimal data for each iteration (instead of none) (used onl-
     $y if ALL=OFF or EVERY=OFF).'/
C
      data LEGS(206),LEGS(207),LEGS(208),LEGS(209),LEGS(210) /
     $'DIANA/ORION line source function calculation dump for transition -
     $(MS/NS): PRD data arrays.',
     $'Save "iterative studies" data for transition (MS/NS).',
     $'Calculate Intensity and Flux emerging from back face of finite at-
     $mosphere.',
     $'Print Sodium populations data.',
     $'Print Calcium populations data.'/
C     !EJECT
      data LEGS(211),LEGS(212),LEGS(213),LEGS(214),LEGS(215) /
     $'Print dump of details of Type-2 Dust Temperature calculation.',
     $'Calculate new Type-2 Dust Temperature table; (used only if DUSTYP-
     $E=ON).',
     $'Print iterative summary of Type-2 Dust Temperature.',
     $'Use Voigt expression for the escape probability.',
     $'Save Lyman-transfer JNU in restart data file.'/
C
      data LEGS(216),LEGS(217),LEGS(218),LEGS(219),LEGS(220) /
     $'Print Continuum Wavelengths summary table (note input parameter I-
     $WSMD).',
     $'Insert place-markers in printout, and generate an index file.',
     $'Print Sobolev integration details, for depth ISOD; (used only if -
     $SOBDMP=ON).',
     $'Include ambipolar diffusion.',
     $'Print details of ambipolar diffusion calculations; (used only if -
     $AMBPRNT=ON).'/
C
      data LEGS(221),LEGS(222),LEGS(223),LEGS(224),LEGS(225) /
     $'Include velocity gradient terms in statistical equilibrium calcul-
     $ations.',
     $'Print details of velocity gradient terms calculations; (used only-
     $ if VLGPRNT=ON).',
     $'Print dump of CO-lines absorption calculation details.',
     $'Print results of continuum calculations for CO-lines wavelengths.-
     $',
     $'Calculate CO-lines cooling rate.'/
C
      data LEGS(226),LEGS(227),LEGS(228),LEGS(229),LEGS(230) /
     $'Print dump of CO-lines cooling rate integration details.',
     $'Print short version of TAU-reduction error messages for WN-matrix-
     $ calculations.',
     $'Print OPTIONS listing.',
     $'Print GTN-editing messages in every iteration, not just the last -
     $one.',
     $'Renormalize helium number densities in the diffusion calculations-
     $.'/
C
      data LEGS(231),LEGS(232),LEGS(233),LEGS(234),LEGS(235) /
     $'Calculate net heating rates.',
     $'Use "in-memory" scratch I/O to the extent possible.',
     $'Do all the normal calculations related to the ion-of-the-run.',
     $'Analyze results of "diffusion" calculation (when AMDIFF=ON or VEL-
     $GRAD=ON)',
     $'Print Voigt profile calculations execution statistics.'/
C
      data LEGS(236),LEGS(237),LEGS(238),LEGS(239),LEGS(240) /
     $'Print performance statistics.',
     $'Bypass the comparative calculation of b-ratios that are not used.-
     $',
     $'Print Oxygen-II populations data.',
     $'Print velocity gradient terms calculation results; (used only whe-
     $n VELGRAD=ON).',
     $'Just read all of the input, then STOP.'/
C     !EJECT
      data LEGS(241),LEGS(242),LEGS(243),LEGS(244),LEGS(245) /
     $'Save data needed by program "CENSUS" for the RABD calculation.',
     $'Print the most detailed execution performance and version descrip-
     $tion data.',
     $'Save data for "Continuum Plots".',
     $'Determine the diffusion term GNV-1 from the non-local N-1 calcula-
     $tion.',
     $'Departure Coefficients calculated from all level equations, rathe-
     $r than from the continuum equation.'/
C
      data LEGS(246),LEGS(247),LEGS(248),LEGS(249),LEGS(250) /
     $'Print S, Rho or J-bar editing messages in every iteration, not ju-
     $st the last one.',
     $'Print ATMOSPHERE data.',
     $'Print ATOM data.',
     $'Print INPUT data.',
     $'Print Line Source Function graphs for all transitions, not just f-
     $or those with LSFPRINT=yes.'/
C
      data LEGS(251),LEGS(252),LEGS(253),LEGS(254),LEGS(255) /
     $'Print Consistency CHECKs from RHO+BD calculation.',
     $'Print BD graphs.',
     $'Print graph of Temperature vs. Optical Depths.',
     $'Print abbreviated results of Continuum Source Function calculatio-
     $n.',
     $'Print abbreviated results of Lyman calculation.'/
C
      data LEGS(256),LEGS(257),LEGS(258),LEGS(259),LEGS(260) /
     $'Print abbreviated populations of the ion of this run.',
     $'Print abbreviated results of HSE calculation.',
     $'Print abbreviated OPTIONS listing.',
     $'Print abbreviated results of Line Source Function calculation.',
     $'Print abbreviated INPUT listing.'/
C
      data LEGS(261),LEGS(262),LEGS(263),LEGS(264),LEGS(265) /
     $'Print abbreviated results of Emergent Line Profile calculation.',
     $'Edit negative line opacity only to keep total opacity positive.',
     $'Replace negative line source function values in "DIRECT" calculat-
     $ions by interpolated values.',
     $'Print ambipolar diffusion calculation results; (used only when AM-
     $DIFF=ON).',
     $'Use calculated diffusion velocity in source function calculations-
     $.'/
C
      data LEGS(266),LEGS(267),LEGS(268),LEGS(269),LEGS(270) /
     $'Do sequential smoothing in diffusion calculations.',
     $'Print "special N-1 calculation" results; (used only if AMBPRNT=ON-
     $).',
     $'Print details of "special N1-calculation"; (used only if ADN1PRNT-
     $=ON).',
     $'Print details of sequential smoothing (used only if SQSMPRNT=ON).-
     $',
     $'Include expansion velocity in the hydrostatic equilibrium equatio-
     $n.'/
C     !EJECT
      data LEGS(271),LEGS(272),LEGS(273),LEGS(274),LEGS(275) /
     $'Do sequential smoothing of departure coefficients.',
     $'Use "special N1-calculation" for ambipolar diffusion, (used only -
     $if AMDIFF=ON) (NOT to be turned off).',
     $'In a Hydrogen run, include collisional broadening for transitions-
     $ above level 5.',
     $'Print summary of Stark splitting of Hydrogen lines.',
     $'Use departure coefficients, instead of number densities, in STIM -
     $for GTN(u,l).'/
C
      data LEGS(276),LEGS(277),LEGS(278),LEGS(279),LEGS(280) /
     $'Print input data and results of charge exchange calculation, (use-
     $d only if CHXCNG=ON).',
     $'Print details of charge exchange calculation, (used only if CHXCN-
     $G=ON).',
     $'Print the bound-bound collision rates CIJ (for minimal printout, -
     $use input parameter IRATE).',
     $'Print the bound-free collision rates PIJ (for minimal printout, u-
     $se input parameter IRATE).',
     $'Print TAU-scales for all transitions with departure coefficients -
     $plots.'/
C
      data LEGS(281),LEGS(282),LEGS(283),LEGS(284),LEGS(285) /
     $'Calculate depth dependence of helium abundance (used only if AMDI-
     $FF=ON).',
     $'Print calculated rates for collisions with Hydrogen atoms.',
     $'Print Iteration Summaries in graphical, rather than tabular, form-
     $ (used only if SUMMARY=ON).',
     $'Print Iteration Trends summary.',
     $'Print frequency integration data for all radiative transitions.'/
C
      data LEGS(286),LEGS(287),LEGS(288),LEGS(289),LEGS(290) /
     $'Print abbreviated frequency integration data.',
     $'Print Line Source Function results for all transitions, not just -
     $for those with LSFPRINT=yes.',
     $'Print dI/dh for emergent continuum intensities, depending on ICDI-
     $T.',
     $'Print and save dI/dh for emergent line intensity profiles.',
     $'Print results in wavenumbers instead of wavelength.'/
C
      data LEGS(291),LEGS(292),LEGS(293),LEGS(294),LEGS(295) /
     $'Print computed effective radiation temperature.',
     $'Print selected ATMOSPHERE input tables again, to more figures.',
     $'Save computed atomic model parameters in output file.',
     $'Save SLF in the Special Spectrum Save file (used only if PROSAV=O-
     $N).',
     $'Print SLF for transition (MS/NS).'/
C
      data LEGS(296),LEGS(297),LEGS(298),LEGS(299),LEGS(300) /
     $'Print graph of SLF for transition (MS/NS).',
     $'Extrapolate input tables to added depths, instead of just extendi-
     $ng the given end values.',
     $'Print input table extrapolation warning message.',
     $'Compute line-free continuum as needed for exhibiting residual lin-
     $e profiles.',
     $'Print results of line-free continuum calculations needed for resi-
     $dual line profiles.'/
C     !EJECT
      data LEGS(301),LEGS(302),LEGS(303),LEGS(304),LEGS(305) /
     $'Print details of calculation of b-ratios from CHI.',
     $'Print iterative summary of CHECK; (used only when CHKGRAF=OFF).',
     $'Calculate the average of the continuum intensities I/Hz.',
     $'Attenuate Hydrogen Stark components outside the Doppler core at e-
     $ach depth.',
     $'Do Continuum Eclipse calculations for CO-lines wavelengths.'/
C
      data LEGS(306),LEGS(307),LEGS(308),LEGS(309),LEGS(310) /
     $'Edit number densities (for positive source functions).',
     $'Use Averaged Line Opacity.',
     $'Print Averaged Line Opacity (note input parameter LWNT).',
     $'Print rates integration summaries.',
     $'Do sequential smoothing for S-from-number densities.'/
C
      data LEGS(311),LEGS(312),LEGS(313),LEGS(314),LEGS(315) /
     $'Print debug details of b-ratios calculation (uses parameter LDINT-
     $).',
     $'Edit input values of RHOWT.',
     $'Compare results from line source function and composite line calc-
     $ulations (see also LINECDMP).',
     $'Print details of scattering albedo analysis (used only when LINEC-
     $OMP=ON).',
     $'Print final sets of Rho and b-ratios from RHO+BD calculation (onl-
     $y if RHBPRNT=ON).'/
C
      data LEGS(316),LEGS(317),LEGS(318),LEGS(319),LEGS(320) /
     $'Try to read all input before stopping because of error.',
     $'Accept mixed-case input statements, instead of upper-case only.',
     $'Use edited TE instead of input values.',
     $'Smooth the nl/n1 ratios in the diffusion calculations.',
     $'Analyze the relative contribution of Composite Line opacity.'/
C
      data LEGS(321),LEGS(322),LEGS(323),LEGS(324),LEGS(325) /
     $'Eliminate TAU less than TSM from emergent intensity and from cont-
     $inuum JNU calculations.',
     $'Print results of continuum calculations for Standard Background w-
     $avelengths.',
     $'Print iterative summary of CHI.',
     $'Print input values of NK, ND and BD.',
     $'Print details of ND and BD calculations at depth # IBNVIEW.'/
C
      data LEGS(326),LEGS(327),LEGS(328),LEGS(329),LEGS(330) /
     $'When mass is prescribed, adjust Z so that computed mass matches i-
     $nput mass table.',
     $'Print Oxygen populations data.',
     $'Use and update CE-enhancement factors when possible and needed.',
     $'Print line profile-specific emergent continuum intensities.',
     $'Use lower-level charge exchange.'/
C
      data LEGS(331),LEGS(332),LEGS(333),LEGS(334),LEGS(335) /
     $'Use upper-level charge exchange.',
     $'Smooth STIM when calculating GTN(u,l).',
     $'Use LTE hydrogen number densities in lower-level charge exchange -
     $calculations.',
     $'Enhance RK artificially (factors RKMULT).',
     $'Compute flow-broadened profiles.'/
C
      data LEGS(336),LEGS(337),LEGS(338),LEGS(339),LEGS(340) /
     $'Print component profiles of flow-broadening.',
     $'Use the Hubeny-Lites, instead of the Kneer-Heasley, formulation f-
     $or PRD.',
     $'Print results of all PRD-iterations, not just the last one.',
     $'Compute H Lyman lines normalizing factors (see also option ULNORM-
     $).',
     $'Use H Lyman lines normalizing factors (see also option CLNORM).'/
C
      data LEGS(341),LEGS(342),LEGS(343),LEGS(344),LEGS(345) /
     $'Print summary of line-center background.',
     $'Print full Line Source Function results.',
     $'Set final negative A-values (frequency integration weight) = 0.',
     $'Use the optically-thin-limit approximation.',
     $'Print Oxygen-III populations data.'/
      data LEGS(346)
     $/'Compute complete set of CI and CE samples.'/
C....$'................. GUIDE .........................................
C.................................................',
C     !EJECT
C
      call HI ('FOP')
C     !BEG
C---- Sort options, leave code in OSCAR
      call WRENCH            (ONAME, IQT, NOOPT, OSCAR, IOPPNT)
C---- Set implied defaults
      call JIGGLE
C---- Set "equivalent" switches
      call BARK
C
      call ZEUS              (NO, IQOPP, LU)
      if(LU.gt.0) then
C----   Print status
        call PRIAM           (LU, 'OPTIONS', 7)
        call LINER           (3, LU)
        write (LU,100)
  100   format(' ','Status of OPTIONS for:')
        call LINER           (1, LU)
        write (LU,101)HEAD
  101   format(' ',A80)
C----   Loop over option types
        do 104 K = 1,4
          NCURR = 0
          call MALACH        (LU, K)
          do 103 J = 1,NOOPT
            read (OSCAR(J),102) KT,I
  102       format(14X,I2,I4)
            if(ONAME(I)(1:4).ne.'0000') then
C----         For actual OPTIONs only
              if(KT.eq.K) then
C----           For current type only
                NCURR = NCURR+1
                call MUCKLE  (I, MLAB)
                call ALYSSUM (LU, NCURR, I, MLAB, LEGS(I))
              end if
            end if
  103     continue
C----     (Force printing of partial last line, if needed)
          call ALYSSUM       (LU, jummy, jummy, 'ZZZZ', qummy)
  104   continue
        call LINER           (2, LU)
        write (LU,105)
  105   format(' ',7X,'Note: * before the option status indicates ',
     $             'that the current status is the default status.'/
     $         ' ',13X,'"status" in lower case indicates input/',
     $             'default setting; "STATUS" in upper case ',
     $             'indicates it was edited.')
      end if
C     !END
      call BYE ('FOP')
C
      return
      end
