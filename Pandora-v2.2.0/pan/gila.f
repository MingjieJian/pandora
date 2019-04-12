      subroutine GILA
     $(TE,HN1,HNK,HE1N1,HE2N1,HE2NK,XNE,W,N,XLR)
C
C     Rudolf Loeser, 1981 Feb 02
C---- Computes reactive thermal conductivity, as given by
C     Nowak and Ulmschneider, Astron.Astrophys. Vol 60, 413 (1977).
C     !DASH
      save
C     !DASH
      real*8 A, ALH, ALHE, B, C, D, HALF, HE1N1, HE2N1, HE2NK, HN1, HNK,
     $       R, RT, S, TE, W, XH, XHE, XHEP, XHEPP, XHP, XLR, XNE, ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
C     !EJECT
      external ZERO1, COULOMB, HI, BYE
C
C               TE(N), HN1(N), HNK(N), HE1N1(N), HE2N1(N), XNE(N), W(N),
      dimension TE(*), HN1(*), HNK(*), HE1N1(*), HE2N1(*), XNE(*), W(*),
C
C               HE2NK(N), XLR(N)
     $          HE2NK(*), XLR(*)
C
      data A,B,C,D /5.208D-12, 6.3114D4, 1.1412D5, 2.5256D5/
C
      call HI ('GILA')
C     !BEG
      call ZERO1       (XLR, N)
C
      do 100 I = 1,N
        S = HE1N1(I)+HE2N1(I)+HE2NK(I)
        if(S.ne.ZERO) then
          ALHE  = S/(S+HN1(I)+HNK(I))
          XHEP  = HE2N1(I)/S
          W(I)  = HALF*ALHE*XHEP
          XHEPP = HE2NK(I)/S
          call COULOMB ('HE+ ', 'HE++', TE(I), XNE(I), R)
          XLR(I) = XLR(I)+W(I)*XHEPP*(((D+TE(I))**2)/R)
        end if
  100 continue
C
      do 101 I = 1,N
        S = HE1N1(I)+HE2N1(I)+HE2NK(I)
        if(S.ne.ZERO) then
          XHE = HE1N1(I)/S
          call COULOMB ('HE+ ', 'HE  ', TE(I), XNE(I), R)
          XLR(I) = XLR(I)+W(I)*XHE*(((C+TE(I))**2)/R)
        end if
  101 continue
C
      do 102 I = 1,N
        S = HN1(I)+HNK(I)
        if(S.ne.ZERO) then
          ALH = S/(S+HE1N1(I)+HE2N1(I)+HE2NK(I))
          XH  = HN1(I)/S
          XHP = HNK(I)/S
          call COULOMB ('H   ', 'H+  ', TE(I), XNE(I), R)
          XLR(I) = XLR(I)+ALH*XH*XHP*(((B+TE(I))**2)/R)
        end if
        RT     = sqrt(TE(I))
        XLR(I) = (A*XLR(I))/(RT**3)
  102 continue
C     !END
      call BYE ('GILA')
C
      return
      end
