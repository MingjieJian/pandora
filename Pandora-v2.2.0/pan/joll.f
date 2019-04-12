      subroutine JOLL
     $(ARR,QAR,KNT)
C
C     Rudolf Loeser, 1985 May 09
C---- Converts SFT into Line Source Function solution type description,
C     for LIZARD.
C     (This is version 2 of JOLL.)
C     !DASH
      save
C     !DASH
      real*8 ARR
      integer I, KNT, LSFT
      character BLANK*1, QAR*10, ZSF*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external HI, BYE
C
C               ARR(8), QAR(16)
      dimension ARR(*), QAR(*)
C
      dimension ZSF(3)
C
      data ZSF /'      Full', '  *Direct*', '  -Escape-'/
C
      call HI ('JOLL')
C     !BEG
      do 100 I = 1,KNT
        LSFT = ARR(I)
C
        if((LSFT.ge.1).and.(LSFT.le.3)) then
          QAR(I) = ZSF(LSFT)
        else if(LSFT.gt.3) then
          QAR(I) = '  nonsense'
        else
          QAR(I) = BLANK
        end if
C
  100 continue
C     !END
      call BYE ('JOLL')
C
      return
      end
