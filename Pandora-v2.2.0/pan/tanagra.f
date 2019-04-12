      subroutine TANAGRA
     $(X,W,IW,N,OPAC,LABEL,TAUK,XMU,Y,MOVING,WN,KODE,WH,ILFLX,IMG,MM,
     $ OPAW,TAUW,WRK,DUMP)
C
C     Rudolf Loeser, 1982 Feb 01
C---- Computes weight matrices for a particular angle in plane-parallel
C     geometry, for angle-dependent continuum calculations.
C     !DASH
      save
C     !DASH
      real*8 OPAC, OPAW, TAUK, TAUW, W, WH, WN, WRK, X, XMU, Y, dummy
      integer I, ILFLX, IMG, IQFIN, IW, KERR, KODE, MM, N, jummy
      logical DUMP, FIN, GDIL, MOVING, TAURED
      character LABEL*100, TITLE*100, qummy*8
C     !COM
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
      equivalence (IQQ( 49),IQFIN)
C     !DASH
      external  AMUCK, LAMBDA, TROYES, MOVED, RAMIREZ, TANG, PHI,
     $          HI, BYE
      intrinsic max, min
C
      dimension X(*), W(*), IW(*)
C
C               TAUK(N,N), OPAC(N,N), WN(N,N), WH(N,N), XMU(1), IMG(N),
      dimension TAUK(N,*), OPAC(N,*), WN(*),   WH(*),   XMU(*), IMG(*),
C
C               OPAW(N), TAUW(N), WRK(N,N)
     $          OPAW(*), TAUW(*), WRK(*)
C
      data TAURED /.true./
      data GDIL   /.false./
C     !EJECT
C
      call HI ('TANAGRA')
C     !BEG
      FIN  = IQFIN.gt.0
      KODE = 1
C
C---- Compute optical depth, TAUK
      do 101 I = 1,N
        call MOVED     (OPAC(I,1), N, N, OPAW, 1, N)
C
        write (LABEL(51:75),100) I
  100   format('Optical Depth for I=',I5)
        call AMUCK     (X, W, OPAW, IMG, LABEL, TAUW, KERR)
        KERR = 1-max(min(KERR,1),0)
        KODE = min(KODE,KERR)
C
        if(DUMP) then
          call RAMIREZ (I, 0, dummy, OPAW, TAUW, N)
        end if
        call TANG      (XMU, 1, TAUW, N, TAUW)
        call MOVED     (TAUW, 1, N, TAUK(I,1), N, N)
  101 continue
C
      if(KODE.eq.1) then
C----   Compute WN
        call LAMBDA    (dummy, W, IW, TAUK, N, N, Y, FIN, TAURED, GDIL,
     $                  jummy, qummy, WN)
        if(ILFLX.gt.0) then
C----     Compute WH
C         (done here because it too needs TAUK)
          call PHI     (TAUK, N, N, Y, FIN, WH, W)
        end if
C
        if(DUMP) then
          write (TITLE,102) MM,XMU(1)
  102     format('Debug Data for ',I2,'. MU value, =',F6.3,'.')
          call TROYES  (N, N, TAUK, N, WN, WH, ILFLX, TITLE, WRK)
        end if
      end if
C
C     !END
      call BYE ('TANAGRA')
C
      return
      end
