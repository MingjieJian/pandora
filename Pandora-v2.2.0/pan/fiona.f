      subroutine FIONA
     $(N,YH,TE,F)
C
C     Rudolf Loeser, 2006 Nov 15
C---- Sets up a rough initial estimate of F, for HEMRE.
C     (This is version 2 of FIONA.)
C     !DASH
      save
C     !DASH
      real*8 BOLZMN, F, FAC, HALF, ONE, TE, YH
      integer I, N
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 2),BOLZMN)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external MOVE1, CONMUL, HI, BYE
C
C               TE(N), F(N)
      dimension TE(*), F(*)
C
      call HI ('FIONA')
C     !BEG
      FAC = BOLZMN*(ONE+HALF+YH)
      call MOVE1  (TE, N, F)
      call CONMUL (FAC, F, N)
C     !END
      call BYE ('FIONA')
C
      return
      end
