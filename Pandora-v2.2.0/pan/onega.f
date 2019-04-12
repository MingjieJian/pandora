      subroutine ONEGA
     $(MO,N,EF,XK,DD,EM,FEM)
C
C     Rudolf Loeser, 2006 Nov 29
C---- Magnetic pressure term printout, for H.S.E.
C     !DASH
      save
C     !DASH
      real*8 DD, EF, EM, FEM, XK
      integer MO, N
      logical EMZERO
      character LAB*36
C     !DASH
      external NAUGHTD, LINER, VECOUT, HI, BYE
C
C               EF(N), XK(N), DD(N), EM(N)
      dimension EF(*), XK(*), DD(*), EM(*)
C
      call HI ('ONEGA')
C     !BEG
      call NAUGHTD  (EM, 1, N, EMZERO)
      if((MO.gt.0).and.(.not.EMZERO)) then
        call LINER  (2, MO)
        write (MO, 100)
  100   format(' ','Calculation of magnetic pressure term, M.')
        call VECOUT (MO, EF, N, 'F = denominator of G')
        call VECOUT (MO, XK, N, 'K = exp(H), where H = integral of G')
        call VECOUT (MO, DD, N, 'D = dPmag/dZ')
        write (LAB,101) FEM
  101   format('M x F, where F =',1PE20.12)
        call VECOUT (MO, EM, N, LAB)
      end if
C     !END
      call BYE ('ONEGA')
C
      return
      end
