      subroutine CALAMUS
     $(NLS,NLY,XLM,T,N,HN,OPACN,SHL,ITAU,DMPI)
C
C     Rudolf Loeser, 2002 Sep 25
C---- Computes higher H Ly lines "background" source function.
C     (This is version 2 of CALAMUS.)
C     !DASH
      save
C     !DASH
      real*8 EX, HN, OPACN, PFAC, SHL, SHLN, T, WLIN, XDEN, XLM, XNUM,
     $       ZERO
      integer ITAU, N, NL, NLS, NLY
      logical DMPI
C     !COM
C---- LIFFEY      as of 2005 Nov 02
      real*8      FLNRML
      dimension   FLNRML(15)
      common      /LIFFEY/ FLNRML
C     Background H Ly alpha & beta normalization factor for the
C     current value of wavelength (only #2 and #3 can differ from 1)
C     (FLNRML is set up by GROAN)
C     .
C---- HILYLI      as of 2005 Dec 22
      integer     LYLINO
      common      /HILYLI/ LYLINO
C     Index of upper level, if this is an H Lyman line wavelength
C     (see subroutine ISTUR).
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external DAMIAN, HILLY, AQUAMA, ALIDUC, HI, BYE
C
C               HN(N,Limp), OPACN(NLY)
      dimension HN(N,*),    OPACN(*)
C
      call HI ('CALAMUS')
C     !BEG
      if(DMPI) then
        call DAMIAN     (XLM, ITAU, T, HN(ITAU,1), NLY, LYLINO)
      end if
C
      SHL = ZERO
      do 100 NL = NLS,NLY
        if(NL.ne.LYLINO) then
          call HILLY    (NL, XLM, T, HN(ITAU,1), HN(ITAU,NL), SHLN,
     $                   WLIN, PFAC, EX, XNUM, XDEN)
          if(DMPI) then
            call AQUAMA (NL, HN(ITAU,NL), XNUM, XDEN, SHLN)
          end if
          SHL = SHL+OPACN(NL)*(SHLN*FLNRML(NL))
        end if
  100 continue
C
      if(DMPI) then
        call ALIDUC     (SHL)
      end if
C     !END
      call BYE ('CALAMUS')
C
      return
      end
