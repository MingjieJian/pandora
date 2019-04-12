      subroutine RAPLOY
     $(KAMB,N,F2C,RHEAB,ABDEL,HND,XNK,XN1,HE2K,HE1)
C
C     Rudolf Loeser, 2000 Jul 18
C---- Computes normalization term F2C, for PYROLA.
C     !DASH
      save
C     !DASH
      real*8 ABDEL, DIV, F2C, HE1, HE2K, HND, RHEAB, XN1, XNK
      integer I, KAMB, N
C     !DASH
      external BRYMBO, DIVIDE, HI, BYE
C
C               XN1(N), RHEAB(N), HE1(N), F2C(N), HE2K(N), ABDEL(N),
      dimension XN1(*), RHEAB(*), HE1(*), F2C(*), HE2K(*), ABDEL(*),
C
C               XNK(N), HND(N)
     $          XNK(*), HND(*)
C
      call HI ('RAPLOY')
C     !BEG
      call BRYMBO   (N,RHEAB,ABDEL)
      do 100 I = 1,N
        if(KAMB.eq.2) then
          DIV = XN1(I)+XNK(I)+HE2K(I)
        else
          DIV = HE1(I)+XN1(I)+XNK(I)
        end if
        call DIVIDE ((ABDEL(I)*HND(I)),DIV,F2C(I))
  100 continue
C     !END
      call BYE ('RAPLOY')
C
      return
      end
