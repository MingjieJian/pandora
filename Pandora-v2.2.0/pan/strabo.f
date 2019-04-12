      subroutine STRABO
     $(KAMB,N,HND,HEND,Y,XN1N)
C
C     Rudolf Loeser, 1998 Feb 17
C---- Computes final N1, for "special N1 calculation" (diffusion).
C     !DASH
      save
C     !DASH
      real*8 HEND, HND, XN1N, Y
      integer KAMB, N
C     !DASH
      external ARRMUL, HI, BYE
C
C               HND(N), HEND(N), Y(N), XN1N(N)
      dimension HND(*), HEND(*), Y(*), XN1N(*)
C
      call HI ('STRABO')
C     !BEG
      if(KAMB.eq.1) then
        call ARRMUL (Y,HND ,XN1N,N)
      else
        call ARRMUL (Y,HEND,XN1N,N)
      end if
C     !END
      call BYE ('STRABO')
C
      return
      end
