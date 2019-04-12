      subroutine AVAM
     $(N,Y,T,S,CNXP,FLX,TF,COEF,G)
C
C     Rudolf Loeser, 1971 Sep 27
C---- Computes the monochromatic flux as a function of depth,
C     using plane-parallel coordinates.
C     !DASH
      save
C     !DASH
      real*8 CNXP, COEF, FAC, FIVE, FLX, G, ONE, OPF, S, T, TF, Y
      integer I, IQFIN, IQIFF, IQINC, IQREF, N
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
      equivalence (RZQ( 41),OPF  )
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
      equivalence (IQQ( 51),IQINC)
      equivalence (IQQ(141),IQIFF)
      equivalence (IQQ( 50),IQREF)
      equivalence (IQQ( 49),IQFIN)
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 6),FIVE  )
C     !DASH
      external ZERO1, ARRINC, SUMPROD, FALCON, GREAT, HI, BYE
C
C               FLX(N), G(N,N), COEF(N), T(N), S(N), CNXP(N)
      dimension FLX(*), G(N,*), COEF(*), T(*), S(*), CNXP(*)
C
      call HI ('AVAM')
C     !BEG
      call ZERO1       (FLX, N)
C
      if(T(2).le.FIVE) then
        call FALCON    (N, Y, T, S, COEF)
        call GREAT     (T, N, Y, IQFIN, IQREF, G)
        do 100 I = 1,N
          call SUMPROD (FLX(I), G(I,1), N, COEF, 1, N)
  100   continue
      end if
C
      if((IQINC.gt.0).and.(IQREF.le.0)) then
        if(IQIFF.gt.0) then
          FAC = -ONE/OPF
        else
          FAC = +ONE/OPF
        end if
        call ARRINC    (CNXP, FAC, FLX, N)
      end if
C
      TF = FLX(1)
C     !END
      call BYE ('AVAM')
C
      return
      end
