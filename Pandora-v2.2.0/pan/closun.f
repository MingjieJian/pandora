      subroutine CLOSUN
     $(X,R)
C
C     Rudolf Loeser, 1998 Jun 22
C---- Encodes X, for printing.
C     !DASH
      save
C     !DASH
      real*8 AX, X, XL, XS
      logical MS
      character R*11
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  HI, BYE
      intrinsic abs
C
      data XS,XL /1.D-3, 1.D4/
C
      call HI ('CLOSUN')
C     !BEG
      MS = X.lt.ZERO
      AX = abs(X)
      if(AX.eq.ZERO) then
        R   = '          0'
      else if(AX.lt.XS) then
        if(MS) then
          R = '       -"0"'
        else
          R = '        "0"'
        end if
      else if(AX.ge.XL) then
        if(MS) then
          R = ' -9999.9999'
        else
          R = '  9999.9999'
        end if
      else
        write (R,100) X
  100   format(F11.4)
      end if
C     !END
      call BYE ('CLOSUN')
C
      return
      end
