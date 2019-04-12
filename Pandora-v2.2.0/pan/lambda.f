      subroutine LAMBDA
     $(X,W,IW,TAU,IMAX,N,Y,FIN,TAURED,GDIL,KODE,TITLE,WN)
C
C     Rudolf Loeser, 1989 Dec 12
C---- Computes the WN matrix: the "Lambda-minus-One" operator.
C     Returns with KODE=1 if all seems OK, with KODE=0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 TAU, TBEG, TEND, W, WN, X, Y
      integer IMAX, IQWDD, IW, KASE, KODE, N
      logical FIN, GDIL, TAURED
      character TITLE*(*)
C     !COM
C---- MUNUXI      as of 2005 Apr 14
      logical     LAMHED,LAMDMP
      common      /MUNUXI/ LAMHED,LAMDMP
C     Subroutine "LAMBDA" extra printout header control.
C     .
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
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
      equivalence (IQQ(146),IQWDD)
C     !DASH
      external QUADRAT, MAPPING, RATRACE, GENERAL, JANITOR, NARIKA,
     $         SECOND, TAFF, MASHED, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               WN(N,N), TAU(N,1) or TAU(N,N)
      dimension WN(*),   TAU(*)
C
      call HI ('LAMBDA')
C     !BEG
      call SECOND      (TBEG)
      call NARIKA      (Y, FIN, KASE)
C
      LAMHED = .false.
      LAMDMP = IQWDD.gt.0
C     !EJECT
      goto (101, 102, 103, 104), KASE
C
C
C----   Methods that employ analytic angle integration -
C       for plane-parallel stationary cases.
C       (Note: QUADRAT, MAPPING and RATRACE use the WNDMP option.)
C
  101   continue
C----     Quadratic representation method - direct
          call QUADRAT (X, TAU, N, Y,      TAURED, GDIL, TITLE, WN,
     $                  W, IW, KODE)
          goto 100
C
  102   continue
C----     Quadratic representation method - mapped
          call MAPPING (X, TAU, N, Y,      TAURED, GDIL, TITLE, WN,
     $                  W, IW, KODE)
          goto 100
C
  103   continue
C----     Ray tracing method
          call RATRACE (X, TAU, N, Y, FIN, TAURED, GDIL, TITLE, WN,
     $                  W,     KODE)
          goto 100
C
C
C----   Method that requires numerical (explicit) angle integration -
C       specifically for spherical and/or moving cases
C       (can also be used for the plane-parallel stationary case).
C       (Note: GENERAL does not use the WNDMP option.)
C
  104   continue
C----     General ray tracing method
          call GENERAL (TAU, IMAX, N, FIN, WN, W, KODE)
          goto 100
C
C
  100 continue
C---- "Clean out junk" (if needed)
      call JANITOR     (WN, N, IMAX)
C
      if(LAMHED) then
C----   "Special printout" closeout
         call MASHED   ('LAMBDA')
      end if
C
      call SECOND      (TEND)
      call TAFF        (1, TBEG, TEND)
C     !END
      call BYE ('LAMBDA')
C
      return
      end
