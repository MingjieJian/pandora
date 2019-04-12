      subroutine GIMEL
     $(Y,S,BHS,B,FLAG,MYS,MSBHS,MBHSB)
C
C     Rudolf Loeser, 1972 Dec 29
C---- Determines relationship symbols for C.S.F. printout.
C     !DASH
      save
C     !DASH
      real*8 B, BHS, S, Y
      integer FLAG
      character EQUAL*1, MBHSB*1, MSBHS*1, MYS*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(44),EQUAL )
C     !DASH
      external GREED, HI, BYE
C
C               MYS(2), MSBHS(2), MBHSB(2)
      dimension MYS(*), MSBHS(*), MBHSB(*)
C
      call HI ('GIMEL')
C     !BEG
      call GREED   (Y,S,MYS)
      call GREED   (BHS,B,MBHSB)
      if(FLAG.gt.0) then
        MSBHS(1) = EQUAL
      else
        call GREED (S,BHS,MSBHS)
      end if
C     !END
      call BYE ('GIMEL')
C
      return
      end
