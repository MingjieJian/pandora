      subroutine WILLET
     $(Z,TE,N,NW,FREQ,WAVE,OP,AA,BB,FINT,FF,GG,W,IMG,ROSSK,ROSST)
C
C     Rudolf Loeser, 1986 Mar 17
C---- Computes Rosseland-mean opacity and optical depth.
C     !DASH
      save
C     !DASH
      real*8 AA, BB, FF, FINT, FREQ, GG, OP, ROSSK, ROSST, TE, W, WAVE,
     $       Z
      integer IMG, IPEX, IQKRD, KODE, LUEO, N, NW
      logical DMP, EDINT, EDTAU
      character LABEL*23, TITA*20, TITB*22
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
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
      equivalence (IQQ(198),IQKRD)
C     !DASH
C     !EJECT
      external CARAWAY, ARRDIV, DANK, CARRACK, MESHED, MASHED, HI, BYE
C
      dimension W(*)
C
C               Z(N), TE(N), FINT(N), OP(N,NW), WAVE(NW), AA(N), BB(N),
      dimension Z(*), TE(*), FINT(*), OP(*),    WAVE(*),  AA(*), BB(*),
C
C               FF(N,NW), GG(N,NW), FREQ(NW), ROSSK(N), ROSST(N),
     $          FF(*),    GG(*),    FREQ(*),  ROSSK(*), ROSST(*),
C
C               IMG(N)
     $          IMG(*)
C
      data TITA  /'numerator of KAPROSS'/
      data TITB  /'denominator of KAPROSS'/
      data LABEL /'Rosseland optical depth'/
C
      call HI ('WILLET')
C     !BEG
      DMP = IQKRD.gt.0
C---- Get integrands FF and GG
      call CARRACK  (NW, FREQ, N, OP, TE, FF, GG)
C---- Integrate FF to get AA
      call CARAWAY  (FF, NW, N, FREQ, WAVE, AA, TITA, DMP)
C---- Integrate GG to get BB
      call CARAWAY  (GG, NW, N, FREQ, WAVE, BB, TITB, DMP)
C---- Compute mean opacity
      call ARRDIV   (AA, BB, ROSSK, N)
C---- Compute optical depth
      call DANK     (1, N, ROSSK, Z, FINT, ROSST, LABEL, KODE, EDINT,
     $               EDTAU, IMG, W)
C
      if((IPEX.lt.0).or.(IPEX.eq.17)) then
        call MESHED ('WILLET', 2)
        write (LUEO,100) KODE,EDINT,EDTAU
  100   format(' ','KODE =',I12,', EDINT =',L3,', EDTAU =',L3)
        call MASHED ('WILLET')
      end if
C     !END
      call BYE ('WILLET')
C
      return
      end
