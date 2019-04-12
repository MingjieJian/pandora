      subroutine PROLAY
     $(KAMB,N,F1C,F1AC,XNK,XN1,HE21,HEK,BETAP)
C
C     Rudolf Loeser, 2000 Jul 18
C---- Computes normalization terms F1C and F1AC, for PYROLA.
C     !DASH
      save
C     !DASH
      real*8 BETAP, F1AC, F1C, HE21, HEK, XN1, XNK
      integer KAMB, N
C     !DASH
      external STABEET, ARRDIV, HI, BYE
C
C               XN1(N), HE21(N), BETAP(N), F1AC(N), XNK(N), F1C(N),
      dimension XN1(*), HE21(*), BETAP(*), F1AC(*), XNK(*), F1C(*),
C
C               HEK(N)
     $          HEK(*)
C
      call HI ('PROLAY')
C     !BEG
      if(KAMB.eq.2) then
        call STABEET (N,XNK,HE21,BETAP)
        call ARRDIV  (BETAP,XNK ,F1C ,N)
        call ARRDIV  (BETAP,HE21,F1AC,N)
      else
        call STABEET (N,HEK,XN1 ,BETAP)
        call ARRDIV  (BETAP,XN1,F1C ,N)
        call ARRDIV  (BETAP,HEK,F1AC,N)
      end if
C     !END
      call BYE ('PROLAY')
C
      return
      end
