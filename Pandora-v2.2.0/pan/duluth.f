      subroutine DULUTH
     $(I,Z,FAC)
C
C     Rudolf Loeser, 1979 Nov 30
C---- Computes the dilution factor for incident radiation,
C     at the I'th depth.
C     (This is version 2 of DULUTH.)
C     !DASH
      save
C     !DASH
      real*8 DLU, F, FAC, ONE, R, R1N, RAT, Z
      integer I, IQRSQ, N
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
      equivalence (RZQ( 17),DLU  )
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
      equivalence (IQQ( 62),IQRSQ)
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
C               Z(N)
      dimension Z(*)
C
      call HI ('DULUTH')
C     !BEG
      if(IQRSQ.gt.0) then
        R = R1N+Z(N)-Z(I)
        call DIVIDE (R1N,R,RAT)
        F = RAT**2
      else
        F = ONE
      end if
C
      FAC = F*DLU
C     !END
      call BYE ('DULUTH')
C
      return
      end
