      subroutine SLEEPY
     $(AUL,PU,XNUU,PL,XNUL,OSF)
C
C     Rudolf Loeser, 1992 Mar 13
C---- Computes oscillator strength.
C     (This is version 2 of SLEEPY.)
C     !DASH
      save
C     !DASH
      real*8 AUL, CON42, DN2, OSF, PL, PU, XNUL, XNUU
      logical KILROY
C     !DASH
      external RIGEL, HI, BYE
C
      data KILROY /.true./
C
      call HI ('SLEEPY')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call RIGEL (42,CON42)
      end if
C
      DN2 = (XNUU-XNUL)**2
      OSF = ((CON42/DN2)*(PU/PL))*AUL
C     !END
      call BYE ('SLEEPY')
C
      return
      end
