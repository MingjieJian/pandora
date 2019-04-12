      subroutine YUKAGIR
     $(Z,XSHL,EMSHL,CSHL,WSHL,LFLX,NSHL,NRPMX)
C
C     Rudolf Loeser, 1981 Oct 26
C---- Computes geometrical quantities for Shell Rays.
C     !DASH
      save
C     !DASH
      real*8 CSHL, EMSHL, R1N, WSHL, XSHL, Z
      integer I, IQTNG, J, LFLX, N, NNS, NRN, NRPMX, NSHL, NTAN
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 23),R1N  )
      equivalence (KZQ( 45),NTAN )
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
      equivalence (IQQ(163),IQTNG)
C     !DASH
C     !EJECT
      external ZERO1, TATAR, CYNTHIA, LEBED, BIHAR, ASSAM, ARRMUL,
     $         HI, BYE
C
C               XSHL(NRPMX,NSHL), EMSHL(N,NSHL), Z(N), WSHL(N,NSHL),
      dimension XSHL(NRPMX,*),    EMSHL(N,*),    Z(*), WSHL(*),
C
C               CSHL(N,NSHL)
     $          CSHL(*)
C
      call HI ('YUKAGIR')
C     !BEG
      NNS = NSHL*N
      NRN = NSHL*NRPMX
      call ZERO1     (XSHL,  NRN)
      call ZERO1     (EMSHL, NNS)
      call ZERO1     (CSHL,  NNS)
C
      I  = 0
      do 100 J = 1,NSHL
        call TATAR   (I)
        call CYNTHIA (I, Z, XSHL (1,J), Z(N), R1N)
        call LEBED   (I, Z, EMSHL(1,J), Z(N), R1N)
  100 contINUE
C
      if((IQTNG.gt.0).and.(NTAN.eq.1)) then
        call ASSAM   (N, NSHL, EMSHL, CSHL)
      else
        call BIHAR   (N, NSHL, EMSHL, CSHL)
      end if
C
      if(LFLX.gt.0) then
        call ARRMUL  (EMSHL, CSHL, WSHL, NNS)
      end if
C     !END
      call BYE ('YUKAGIR')
C
      return
      end
