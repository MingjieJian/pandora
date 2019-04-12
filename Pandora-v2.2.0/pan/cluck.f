      subroutine CLUCK
     $(ARR,QAR,KNT)
C
C     Rudolf Loeser, 1986 Jun 03
C---- Converts FDB into Source Function background description,
C     for LIZARD.
C     !DASH
      save
C     !DASH
      real*8 ARR
      integer I, IFDB, KNT
      character BLANK*1, QAR*10, ZDB*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  HI, BYE
      intrinsic min
C
C               ARR(8), QAR(16)
      dimension ARR(*), QAR(*)
C
      dimension ZDB(3)
C
      data ZDB /'  Constant', '   Varying', '  nonsense'/
C
      call HI ('CLUCK')
C     !BEG
      do 100 I = 1,KNT
        IFDB = ARR(I)
        IFDB = min(IFDB,3)
C
        if(IFDB.ge.1) then
          QAR(I) = ZDB(IFDB)
        else
          QAR(I) = BLANK
        end if
C
  100 continue
C     !END
      call BYE ('CLUCK')
C
      return
      end
