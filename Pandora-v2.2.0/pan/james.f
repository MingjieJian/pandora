      subroutine JAMES
     $(X,W,N,Z,F,R,LABEL,IMG)
C
C     Rudolf Loeser, 2006 Nov 28
C---- Computes R = integral(F), for H.S.E.
C     (This is version 4 of JAMES.)
C     !DASH
      save
C     !DASH
      real*8 CMPKM, F, R, W, X, Z
      integer IFINT, IMG, IN, IS, MOX, N, jummy
      logical lummy1, lummy2
      character LABEL*100
C     !COM
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
      external LAUD, DANK, CONDIV, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               Z(N), F(N), R(N), IMG(N)
      dimension Z(*), F(*), R(*), IMG(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1), IFINT )
C
      call HI ('JAMES')
C     !BEG
C     (Get W allotment)
      call LAUD   (IN, IS, MOX, 'JAMES')
C
      call DANK   (1, N, F, Z, W(IFINT), R, LABEL, jummy, lummy1,
     $             lummy2, IMG, W)
      call CONDIV (CMPKM, R, N)
C
C     (Give back W allotment)
      call WGIVE  (W, 'JAMES')
C     !END
      call BYE ('JAMES')
C
      return
      end
