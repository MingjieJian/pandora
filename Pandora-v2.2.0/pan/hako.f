      subroutine HAKO
     $(Y,TIT)
C
C     Rudolf Loeser, 1984 Dec 21
C---- Converts Y into a method description.
C     (This is version 2 of HAKO.)
C     !DASH
      save
C     !DASH
      real*8 ONE, THREE, TWO, Y, ZERO
      integer I
      character METH*10, TIT*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
C     !DASH
      external  HI, BYE
C
      dimension METH(6)
C
      data METH /'        RT', ' Mapped QR', '        GR',
     $           '(not used)', '        QR', '        ??'/
C
      call HI ('HAKO')
C     !BEG
      if(Y.eq.TWO) then
        I =  4
      else if((Y.ge.ZERO).and.(Y.le.ONE)) then
        I =  5
      else if((Y.ge.-THREE).and.(Y.le.-ONE)) then
        I = -Y
      else
        I =  6
      end if
C
      TIT = METH(I)
C
      if(I.eq.5) then
        write (TIT(2:7),100) Y
  100   format(F6.4)
      else if(I.eq.6) then
        write (TIT(1:8),101) Y
  101   format(1PE8.0)
      end if
C     !END
      call BYE ('HAKO')
C
      return
      end
