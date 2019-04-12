      subroutine ROVER
     $(N,NL,NLB,LU,IQCSW,XNUK,XNU,TE,TR,ALF,BATA,BATAR,BATAL)
C
C     Rudolf Loeser, 1982 Feb 19
C---- Computes and prints Stimulated Emission Factors, and Alphas.
C     (This is version 2 of ROVER.)
C     !DASH
      save
C     !DASH
      real*8 ALF, BATA, BATAL, BATAR, TE, TR, XNU, XNUK
      integer IQCSW, LU, N, NL, NLB
C     !DASH
      external MESSENE, PYRGOS, BUZZARD, CHICKEN, HI, BYE
C
C               BATAL(N,NLB), BATAR(N,MUL), TE(N), ALF(MUL), TR(N,NSL),
      dimension BATAL(*),     BATAR(*),     TE(*), ALF(*),   TR(*),
C
C               XNU(NSL), BATA(N,MUL)
     $          XNU(*),   BATA(*)
C
      call HI ('ROVER')
C     !BEG
C---- Compute Alphas
      call MESSENE  (N, NL, XNU, ALF)
C
C---- Compute S.E.F.(TE), for transitions
      call PYRGOS   (N, NL, TE, XNU, BATA )
      if(IQCSW.gt.0) then
C----   Compute S.E.F.(TR), for transitions
        call PYRGOS (N, NL, TR, XNU, BATAR)
      end if
C
C---- Compute S.E.F. for levels
      call BUZZARD  (N, NL, NLB, TE, XNU, XNUK, BATAL)
C
C---- Print
      call CHICKEN  (LU, N, NL, NLB, IQCSW, ALF, BATA, BATAR, BATAL)
C     !END
      call BYE ('ROVER')
C
      return
      end
