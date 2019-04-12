      subroutine RAYSCAT
     $(INDX,XLM,N,NOPAC,HN,CONT)
C
C     Rudolf Loeser, 1988 Oct 26
C---- Computes a set of Hydrogen Rayleigh scattering opacity values.
C     (This is version 4 of RAYSCAT.)
C     !DASH
      save
C     !DASH
      real*8 CONT, HN, XLM
      integer I, INDX, N, NOPAC
C     !DASH
      external MILDRED, HI, BYE
C
C               HN(N,Limp), CONT(Nopac,N)
      dimension HN(N,*),    CONT(NOPAC,*)
C
      call HI ('RAYSCAT')
C     !BEG
      do 100 I = 1,N
        call MILDRED (HN(I,1), XLM, CONT(INDX,I))
  100 continue
C     !END
      call BYE ('RAYSCAT')
C
      return
      end
