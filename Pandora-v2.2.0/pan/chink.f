      subroutine CHINK
     $(NE,RAT,XLB,SP,V,WN,XLF)
C
C     Rudolf Loeser, 2002 Apr 19
C---- Adds the K'th increment to XLF, for LAUGH.
C     (This is version 2 of CHINK.)
C     !DASH
      save
C     !DASH
      real*8 FAC, RAT, SP, V, WN, XLB, XLF
      integer I, J, NE
C     !DASH
      external HI, BYE
C
C               XLB(NE), SP(NE), V(NE), WN(NE,NE), XLF(NE)
      dimension XLB(*),  SP(*),  V(*),  WN(NE,*),  XLF(*)
C
      call HI ('CHINK')
C     !BEG
      do 101 J = 1,NE
        FAC = XLB(J)*SP(J)*RAT
        do 100 I = 1,NE
          XLF(I) = XLF(I)+FAC*V(I)*WN(I,J)
  100   continue
  101 continue
C     !END
      call BYE ('CHINK')
C
      return
      end
