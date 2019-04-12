      subroutine TRECK
     $(ARR,QAR,KNT)
C
C     Rudolf Loeser, 1984 Aug 09
C---- Converts "UIR" into alphabetic message regarding use of input Rho,
C     for LIZARD.
C     !DASH
      save
C     !DASH
      real*8 ARR
      integer I, IUIR, KNT
      character QAR*10, SIG*10
C     !DASH
      external HI, BYE
C
C               ARR(8), QAR(16)
      dimension ARR(*), QAR(*)
C
      dimension SIG(3)
C
      data SIG /'          ', '       Yes', '     Huh ?'/
C
      call HI ('TRECK')
C     !BEG
      do 100 I = 1,KNT
        QAR(I) = SIG(1)
C
        IUIR = ARR(I)
        if(IUIR.gt.0) then
          IUIR = IUIR-1
          if((IUIR.lt.0).or.(IUIR.gt.1)) then
            IUIR = 2
          end if
          QAR(I) = SIG(IUIR+1)
        end if
C
  100 continue
C     !END
      call BYE ('TRECK')
C
      return
      end
