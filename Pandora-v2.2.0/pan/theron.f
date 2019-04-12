      subroutine THERON
     $(INDX,NOPAC,N,OPAC,RSC,CO)
C
C     Rudolf Loeser, 2004 Aug 31
C---- Computes CO-lines absorption contribution.
C     (This is version 2 of THERON.)
C     !DASH
      save
C     !DASH
      real*8 CO, OPAC, RSC
      integer I, INDX, N, NOPAC
C     !DASH
      external HI, BYE
C
C               CO(Nopac,N), OPAC(N), RSC(N)
      dimension CO(NOPAC,*), OPAC(*), RSC(*)
C
      call HI ('THERON')
C     !BEG
      do 100 I = 1,N
        CO(INDX,I) = OPAC(I)*RSC(I)
  100 continue
C     !END
      call BYE ('THERON')
C
      return
      end
