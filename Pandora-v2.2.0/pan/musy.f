      subroutine MUSY
     $(LU,IBNVW,N,NL,IQBDC,IQAMD,IQVLG,IQBED,IQBSM,NBS,WBDIR,WBD,
     $ BD0,BD1,BDN,BDD,BDR,BDE,BDIUW,BDIW,BDA)
C
C     Rudolf Loeser, 2003 Jun 27
C---- Prints B-development trace for MYTU.
C     !DASH
      save
C     !DASH
      real*8 BD0, BD1, BDA, BDD, BDE, BDIUW, BDIW, BDN, BDR, WBD, WBDIR,
     $       dummy
      integer IBNVW, IQAMD, IQBDC, IQBED, IQBSM, IQVLG, LU, N, NBS, NL
      character LBD0*15, LBD1*15, LBDA*15, LBDD*15, LBDE*15, LBDN*15,
     $          LBDR*15, LBDU*15, LBDW*15, qummy*8
C     !DASH
      external LINER, MOTE, HI, BYE
C
C               BDN(N,NL), BDIW(N,NL), BDA(N,NL), BD0(N,NL), BD1(N,NL),
      dimension BDN(N,*),  BDIW(N,*),  BDA(N,*),  BD0(N,*),  BD1(N,*),
C
C               BDIUW(N,NL), BDD(N,NL), BDR(N,NL), BDE(N,NL)
     $          BDIUW(N,*),  BDD(N,*),  BDR(N,*),  BDE(N,*)
C
      data LBD0,LBD1,LBDN,LBDD,LBDR,LBDE,LBDU,LBDW,LBDA
     $         /'   BD0',          '   BD1',          '   BD(normal)',
     $          '   BD(direct)',   '   BD(raw)',      '   BD(edited)',
     $          '   BD(smoothed)', '   BD(weighted)', '   BD(new)'/
C
      call HI ('MUSY')
C     !BEG
      if(LU.gt.0) then
        call LINER  (1,LU)
        write (LU,100)
  100   format(' ','The final values of BD(new) are obtained from ',
     $             'an initial, provisional computed set ',
     $             'called BD(raw);'/
     $         ' ','these values can then be edited, smoothed, and ',
     $             'weighted as explained here.'//
     $         ' ','Two versions of BD(raw) can be computed: ',
     $             'BD(normal) and BD(direct).'//
     $         ' ','Two versions of BD(normal) are computed: ',
     $             'BD0 and BD1; option BDCALC selects one of them.'/
     $         ' ','BD0 is based on BDratio and GNV (if it exists);'/
     $         ' ','BD1 is based on RHO (see preceding output ',
     $             'section RHO AND RBD) and GNV (if it exists).')
        if(IQBDC.gt.0) then
          write (LU,101)
  101     format(' ','   Since BDCALC = on, BD(normal) = BD1.')
        else
          write (LU,102)
  102     format(' ','   Since BDCALC = off, BD(normal) = BD0.')
        end if
        call MOTE   (LU, NL, 3, 0, IBNVW, N, BD0, LBD0, BD1, LBD1,
     $               BDN, LBDN)
C     !EJECT
        call LINER   (1, LU)
        write (LU,103)
  103   format(' ','BD-1(direct), for level 1, is based on N1(old), ',
     $             'NK(old), and NE;'/
     $         ' ','BD-m(direct), for higher levels, = ',
     $             'BDratio-m * BD-1(direct).'/
     $         ' ','BD(direct) is computed only when either option ',
     $             'AMDIFF = on and/or option VELGRAD = on.')
        if((IQAMD.gt.0).or.(IQVLG.gt.0)) then
          write (LU,104)
  104     format(' ','   BD(direct) was computed in this run.')
          call MOTE  (LU, NL, 1, 0, IBNVW, N, BDD, LBDD, dummy, qummy,
     $                dummy, qummy)
          call LINER (1, LU)
          write (LU,105) WBDIR
  105     format(' ','BD(raw) = BD(direct) [ WT * WBDIR ] + ',
     $               'BD(normal) [ 1 - WT * WBDIR ]; WBDIR =',1PE12.5)
          call MOTE  (LU, NL, 1, 0, IBNVW, N, BDR, LBDR, dummy, qummy,
     $               dummy, qummy)
        else
          write (LU,106)
  106     format(' ','   BD(direct) was not computed in this run; ',
     $               'thus BD(raw) just = BD(normal).')
          call MOTE (LU, NL, 1, 0, IBNVW, N, BDR, LBDR, dummy, qummy,
     $               dummy, qummy)
        end if
C
        call LINER   (1, LU)
        write (LU,107)
  107   format(' ','BD(raw) was edited as necessary to remove values ',
     $             'less than or equal to zero.')
        call MOTE    (LU, NL, 1, 0, IBNVW, N, BDE, LBDE, dummy, qummy,
     $                dummy, qummy)
C
        call LINER   (1, LU)
        write (LU,108)
  108   format(' ','BD(edited) can be smoothed, for levels NBS and ',
     $             'higher, depending on option BSMOOTH.')
        if(IQBSM.gt.0) then
          write (LU,109) NBS
  109     format(' ','   Since BSMOOTH = on, results for level NBS =',
     $               I3,' and higher were smoothed (default value ',
     $               'of NBS = 2).')
        else
          write (LU,110)
  110     format(' ','   Since BSMOOTH = off, the value of ',
     $               'BD(smoothed) just = BD(edited).')
        end if
        call MOTE    (LU, NL, 1, 0, IBNVW, N, BDIUW, LBDU, dummy, qummy,
     $                dummy, qummy)
C     !EJECT
        call LINER  (1, LU)
        write (LU,111) WBD
  111   format(' ','BD(smoothed) was weighted with BD(old) using ',
     $             'WBD =',1PE12.5,' [default value of WBD = WPOP ',
     $             '(see below)].')
        call MOTE   (LU, NL, 1, 0, IBNVW, N, BDIW, LBDW, dummy, qummy,
     $               dummy, qummy)
C
        call LINER  (1, LU)
        write (LU,112)
  112   format(' ','These weighted values can be edited to prevent ',
     $             'negative continuum source functions, depending ',
     $             'on option BEDIT.')
        if(IQBED.gt.0) then
          write (LU,113)
  113     format(' ','   Since BEDIT = on, values of BD(weighted) ',
     $               'less than or equal to beta were replaced by ',
     $               'larger ones, giving BD(new).')
        else
          write (LU,114)
  114     format(' ','   Since BEDIT = off, BD(new) just = BD(edited).')
        end if
        call MOTE   (LU, NL, 1, 0, IBNVW, N, BDA, LBDA, dummy, qummy,
     $               dummy, qummy)
      end if
C     !END
      call BYE ('MUSY')
C
      return
      end
