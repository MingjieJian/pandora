      subroutine ELDONIA
     $(TRI,RKI,KNT,TRMN,TRMX)
C
C     Rudolf Loeser, 1991 Mar 25
C---- Rearranges the TRI and RKI tables so that TRMN and TRMX
C     can be picked, for ADDER.
C     !DASH
      save
C     !DASH
      real*8 RKI, TRI, TRMN, TRMX
      integer I, J, KNT, LIM
C     !DASH
      external  SWAPD, HI, BYE
      intrinsic abs, min, max
C
C               TRI(KNT), RKI(KNT)
      dimension TRI(*),   RKI(*)
C
      call HI ('ELDONIA')
C     !BEG
      LIM = KNT
C
      do 101 J = 1,2
C
        LIM = LIM-1
        do 100 I = 1,LIM
          if(abs(RKI(I)).lt.abs(RKI(I+1))) then
            call SWAPD (RKI(I),1,RKI(I+1),1)
            call SWAPD (TRI(I),1,TRI(I+1),1)
          end if
  100   continue
C
  101 continue
C
      TRMN = min(TRI(KNT),TRI(KNT-1))
      TRMX = max(TRI(KNT),TRI(KNT-1))
C     !END
      call BYE ('ELDONIA')
C
      return
      end
