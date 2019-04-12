      subroutine MUAR
     $(NW,N,NCP,OPAC,COMP,CORC)
C
C     Rudolf Loeser, 2002 Oct 04
C---- Computes the Composite Line Opacity ratio, for PINANG.
C     !DASH
      save
C     !DASH
      real*8 COMP, CORC, OPAC
      integer I, N, NCP, NW
C     !DASH
      external DIVIDE, HI, BYE
C
C               COMP(NCP,N), CORC(NCP,N), OPAC(N)
      dimension COMP(NCP,*), CORC(NCP,*), OPAC(*)
C
      call HI ('MUAR')
C     !BEG
      do 100 I = 1,N
        call DIVIDE (COMP(NW,I),OPAC(I),CORC(NW,I))
  100 continue
C     !END
      call BYE ('MUAR')
C
      return
      end
