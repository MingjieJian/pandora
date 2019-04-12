      subroutine NEMAHA
     $(KAMB,N,XNK,HEK,HE21,XNKU)
C
C     Rudolf Loeser, 1990 Apr 30
C---- Computes "XNK-used" for diffusion calculations.
C     !DASH
      save
C     !DASH
      real*8 HE21, HEK, XNK, XNKU
      integer KAMB, N
C     !DASH
      external ARRAVE, MOVE1, HI, BYE
C
C               XNK(N), XNKU(N), HEK(N), HE21(N)
      dimension XNK(*), XNKU(*), HEK(*), HE21(*)
C
      call HI ('NEMAHA')
C     !BEG
      if(KAMB.eq.2) then
        call ARRAVE (HEK,HE21,XNKU,N)
      else
        call MOVE1  (XNK,N,XNKU)
      end if
C     !END
      call BYE ('NEMAHA')
C
      return
      end
