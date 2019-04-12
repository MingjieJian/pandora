      subroutine BONOR
     $(IU,IL,P,TE,CE)
C
C     Rudolf Loeser, 2007 Mar 26
C---- Computes defaults values of CE for Hydrogen
C     (valid for IU .le. 7).   Uses data provided by
C
C     N. Przybilla & K. Butler (2004), ApJ, 609, 1181.
C
C     !DASH
      save
C     !DASH
      real*8 CE, FAC, P, RT, TE, UPSILON, ZERO
      integer IL, IU
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external BELTIR, HI, BYE
C
C               P(NSL)
      dimension P(*)
C
      data FAC /8.629D-6/
C
      call HI ('BONOR')
C     !BEG
      call BELTIR (IU, IL, TE, UPSILON)
C
      if(UPSILON.ne.ZERO) then
        RT = sqrt(TE)
        CE = (FAC*UPSILON)/(RT*P(IL))
C
      else
        CE = ZERO
      end if
C     !END
      call BYE ('BONOR')
C
      return
      end
