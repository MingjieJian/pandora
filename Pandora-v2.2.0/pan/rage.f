      subroutine RAGE
     $(TAU,N,EXT)
C
C     Rudolf Loeser, 1980 Apr 21
C---- Computes the extinction function for incident radiation.
C     (This is version 2 of RAGE.)
C     !DASH
      save
C     !DASH
      real*8 EB, EF, EXT, OPF, TAU, TAUMAX, TWO, X, XB, XF, ZERO, dummy
      integer I, IQIFF, IQREF, N
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
      equivalence (IQQ(141),IQIFF)
      equivalence (IQQ( 50),IQREF)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
C     !EJECT
      external EXPINT, HI, BYE
C
C               TAU(N), EXT(N)
      dimension TAU(*), EXT(*)
C
      call HI ('RAGE')
C     !BEG
      TAUMAX = TAU(N)
      if(IQREF.gt.0) then
        TAUMAX = TWO*TAU(N)
      end if
C
      do 100 I = 1,N
        XF = OPF*TAU(I)
        XB = OPF*(TAUMAX-TAU(I))
C
        if(IQREF.gt.0) then
          call EXPINT   (2, XF, EF, dummy)
          call EXPINT   (2, XB, EB, dummy)
        else
          if(IQIFF.gt.0) then
            call EXPINT (2, XF, EF, dummy)
            EB = ZERO
          else
            EF = ZERO
            call EXPINT (2, XB, EB, dummy)
          end if
        end if
C
        EXT(I) = EF+EB
  100 continue
C     !END
      call BYE ('RAGE')
C
      return
      end
