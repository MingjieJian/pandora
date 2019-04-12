      subroutine HEMRE
     $(KMASN,N,YH,GMASIN,TE,HND,XNE,F,G)
C
C     Rudolf Loeser, 2003 Nov 04
C---- Initializes, when input mass is given.
C     !DASH
      save
C     !DASH
      real*8 F, G, GMASIN, HALF, HND, TE, XNE, YH
      integer KMASN, N
      logical FLAG
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C     !DASH
      external FIONA, NAUGHTD, PAYDOR, ARRDIV, MOVE1, CONMUL, HI, BYE
C
C               GMASIN(N), HND(N), XNE(N), TE(N), G(N), F(N)
      dimension GMASIN(*), HND(*), XNE(*), TE(*), G(*), F(*)
C
      call HI ('HEMRE')
C     !BEG
      if(KMASN.gt.0) then
C
        call NAUGHTD  (HND, 1, N, FLAG)
        if(FLAG) then
C----     Get F and G
          call FIONA  (N, YH, TE, F)
          call PAYDOR (GMASIN, N, G)
C----     Get HND
          call ARRDIV (G, F, HND, N)
        end if
C
        call NAUGHTD  (XNE, 1, N, FLAG)
        if(FLAG) then
C----     Get XNE
          call MOVE1  (HND, N, XNE)
          call CONMUL (HALF, XNE, N)
        end if
C
      end if
C     !END
      call BYE ('HEMRE')
C
      return
      end
