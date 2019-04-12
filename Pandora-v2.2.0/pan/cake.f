      subroutine CAKE
     $(N,TAUKIN,PREF,F,Z,W)
C
C     Rudolf Loeser, 1994 May 17
C---- Computes Z from TAUKIN and PREF.
C     (This is version 5 of CAKE.)
C     !DASH
      save
C     !DASH
      real*8 CMPKM, DIV, F, ONE, PREF, TAUKIN, W, Z
      integer I, N
      logical DUMP
      character LABEL*100
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 5),CMPKM )
C     !DASH
      external DIVIDE, PAMPAS, HI, BYE
C
      dimension W(*)
C
C               TAUKIN(N), PREF(N), F(N), Z(N)
      dimension TAUKIN(*), PREF(*), F(*), Z(*)
C
      data LABEL /'Z-from-TAUKIN'/
      data DUMP  /.false./
C
      call HI ('CAKE')
C     !BEG
      do 100 I = 1,N
        DIV = CMPKM*PREF(I)
        call DIVIDE (ONE, DIV, F(I))
  100 continue
C
      call PAMPAS   (N, TAUKIN, F, Z, LABEL, DUMP, W)
C     !END
      call BYE ('CAKE')
C
      return
      end
