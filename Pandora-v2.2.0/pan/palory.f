      subroutine PALORY
     $(KAMB,N,F,HE21,HE2K,HE1,HEK,SHE,SHE2,PNF)
C
C     Rudolf Loeser, 2000 Jul 18
C---- Applies normalization factor, for PYROLA.
C     !DASH
      save
C     !DASH
      real*8 F, HE1, HE21, HE2K, HEK, PNF, SHE, SHE2
      integer KAMB, N
C     !DASH
      external ARRMUL, HI, BYE
C
C               HE21(N), HE2K(N), HE1(N), HEK(N), SHE(N), PNF(N), F(N),
      dimension HE21(*), HE2K(*), HE1(*), HEK(*), SHE(*), PNF(*), F(*),
C
C               SHE2(N)
     $          SHE2(*)
C
      call HI ('PALORY')
C     !BEG
      if(KAMB.eq.2) then
        call ARRMUL (HE21,F,HE21,N)
        call ARRMUL (HE2K,F,HE2K,N)
        call ARRMUL (SHE2,F,SHE2,N)
      else
        call ARRMUL (HE1 ,F,HE1 ,N)
        call ARRMUL (HEK ,F,HEK ,N)
        call ARRMUL (SHE ,F,SHE ,N)
      end if
      call ARRMUL   (PNF ,F,PNF ,N)
C     !END
      call BYE ('PALORY')
C
      return
      end
