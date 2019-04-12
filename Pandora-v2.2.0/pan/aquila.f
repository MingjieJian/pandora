      subroutine AQUILA
     $(N,Z,ZI,Z1,Z2,Z3,ZT,DEE,VAMB,VBMB,VCMB,VDMB,VEMB,W,IW)
C
C     Rudolf Loeser, 1989 Sep 15
C---- Supervises calculation of velocities for diffusion calculations.
C     (This is version 2 of AQUILA.)
C     !DASH
      save
C     !DASH
      real*8 DEE, VAMB, VBMB, VCMB, VDMB, VEMB, W, Z, Z1, Z2, Z3, ZI,
     $       ZT
      integer I, INDX, IQDSM, IW, N, jummy
      logical KILROY, lummy
      character BLANK*1, LABEL*100, TYPE*3
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
      equivalence (IQQ(266),IQDSM)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external LEO, SMOOTH, HI, BYE
C
      dimension W(*), IW(*)
C
C               ZI(N), Z1(N), Z2(N), Z3(N), ZT(N), DEE(4,5,N), VEMB(N),
      dimension ZI(*), Z1(*), Z2(*), Z3(*), ZT(*), DEE(4,5,*), VEMB(*),
C
C               VAMB(N), VBMB(N), VCMB(N), VDMB(N), Z(N)
     $          VAMB(*), VBMB(*), VCMB(*), VDMB(*), Z(*)
C
      data TYPE,INDX /'lin', 0/
C     !EJECT
C
      call HI ('AQUILA')
C     !BEG
C---- Compute velocities
      do 100 I = 1,N
        call LEO    (ZI(I), Z1(I), Z2(I), Z3(I), ZT(I), DEE(1,1,I),
     $               VAMB(I), VBMB(I), VCMB(I), VDMB(I), VEMB(I))
  100 continue
C
      if(IQDSM.gt.0) then
C----   Sequential smoothing
        KILROY = .true.
C
        LABEL = 'Diffusion velocity VAMB'
        call SMOOTH (Z, VAMB, N, TYPE, LABEL, INDX, W, IW, jummy,
     $               lummy)
C
        LABEL = 'Diffusion velocity VBMB'
        call SMOOTH (Z, VBMB, N, TYPE, LABEL, INDX, W, IW, jummy,
     $               lummy)
C
        LABEL = 'Diffusion velocity VCMB'
        call SMOOTH (Z, VCMB, N, TYPE, LABEL, INDX, W, IW, jummy,
     $               lummy)
C
        LABEL = 'Diffusion velocity VDMB'
        call SMOOTH (Z, VDMB, N, TYPE, LABEL, INDX, W, IW, jummy,
     $               lummy)
C
        LABEL = 'Diffusion velocity VEMB'
        call SMOOTH (Z, VEMB, N, TYPE, LABEL, INDX, W, IW, jummy,
     $               lummy)
      end if
C     !END
      call BYE ('AQUILA')
C
      return
      end
