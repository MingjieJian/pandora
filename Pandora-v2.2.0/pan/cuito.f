      subroutine CUITO
     $(XLM,TE,H1,CALL,FALL,TERM)
C
C     Rudolf Loeser, 2002 Dec 03
C---- Computes new TERM for H Lyman alpha wing background opacity.
C     !DASH
      save
C     !DASH
      real*8 CALL, F12, FAC, FALL, H1, PI, REL, TE, TERM, XLM
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 1),PI    )
C     !DASH
      external REAPER, HI, BYE
C
      data F12,REL,FAC /4.161543D-1, 2.818D-13, 1.D-17/
C
      call HI ('CUITO')
C     !BEG
      call REAPER (TE,XLM,FALL)
      CALL = FAC*PI*F12*REL
      TERM = CALL*FALL*(H1**2)
C     !END
      call BYE ('CUITO')
C
      return
      end
