      subroutine SPUME
     $(X,I,Z,MODE,NODE,XINK,FINK,TERM)
C
C     Rudolf Loeser, 1971 Dec 15
C---- Computes the Incident Radiation term.
C     If MODE=1, then X is in Frequency Units;
C     if MODE=2, then X is in Angstroms;
C     if NODE=1, then TERM is the regular incident radiation term;
C     if NODE=2, then TERM is the incident radiation term without Alpha.
C     (This is version 2 of SPUME.)
C     !DASH
      save
C     !DASH
      real*8 DLU, FAL, FINK, FNU, RAD, TERM, X, XINK, Z, ZERO
      integer I, IQINC, MODE, NODE
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
      equivalence (IQQ( 51),IQINC)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external SPIFF, CLIFF, DULUTH, TOTS, HI, BYE
C
C               Z(N), XINK(INK), FINK(INK)
      dimension Z(*), XINK(*),   FINK(*)
C     !EJECT
C
      call HI ('SPUME')
C     !BEG
      if(IQINC.le.0) then
C
        TERM = ZERO
C
      else
C
C----   Make sure FNU is in Frequency Units
        call SPIFF  (MODE,X,FNU)
C----   Get "Alpha"
        call TOTS   (NODE,FNU,FAL)
C----   Get incident radiation
        call CLIFF  (FNU,XINK,FINK,RAD)
C----   Get Dilution Factor
        call DULUTH (I,Z,DLU)
C----   Combine factors
C
        TERM = DLU*RAD*FAL
C
      end if
C     !END
      call BYE ('SPUME')
C
      return
      end
