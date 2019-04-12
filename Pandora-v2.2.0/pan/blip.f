      subroutine BLIP
     $(X,W,N,COP,GTO,TAUM,IMG,PHM,GTM,EDINT,EDTAU)
C
C     Rudolf Loeser, 2006 Feb 27
C---- Computes mean-TAU (computed with phi = 1).
C     (This is version 2 of BLIP.)
C     !DASH
      save
C     !DASH
      real*8 COP, GTM, GTO, ONE, PHM, TAUM, W, X
      integer IMG, N
      logical EDINT, EDTAU, lummy
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external SET1, DEODAR, MYNAH, HI, BYE
C
      dimension X(*), W(*)
C
C               TAUM(N), COP(N), GTO(N), PHM(N), GTM(N), IMG(N)
      dimension TAUM(*), COP(*), GTO(*), PHM(*), GTM(*), IMG(*)
C
      call HI ('BLIP')
C     !BEG
      call SET1   (PHM, N, ONE)
      call DEODAR (N, GTO, GTM, PHM, COP, lummy, IMG)
      call MYNAH  (X, W, PHM, COP, GTM, TAUM, EDINT, EDTAU, IMG, 1)
C     !END
      call BYE ('BLIP')
C
      return
      end
