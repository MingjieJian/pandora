      subroutine DARIEN
     $(XL,XR,ER,MR)
C
C     Rudolf Loeser, 1989 Dec 21
C---- Compares, for NUFU.
C     !DASH
      save
C     !DASH
      real*8 XL, XR, ZERO
      character BLANK*1, EQUAL*1, ER*(*), MARK*1, MR*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(44),EQUAL )
C     !DASH
      external  HI, BYE
C
      call HI ('DARIEN')
C     !BEG
      if(XR.eq.ZERO) then
        MARK = BLANK
      else
        if(XR.eq.XL) then
          MARK = EQUAL
        else if(XR.gt.XL) then
          MARK = '<'
        else
          MARK = '>'
        end if
      end if
C
      if(MARK.eq.MR) then
        MARK = BLANK
      else
        MR = MARK
      end if
      ER(2:2) = MARK
C     !END
      call BYE ('DARIEN')
C
      return
      end
