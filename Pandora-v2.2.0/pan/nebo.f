      subroutine NEBO
     $(X,P)
C
C     Rudolf Loeser, 2000 Oct 24
C---- Encodes, for THUNDER headers.
C     !DASH
      save
C     !DASH
      real*8 HNDRD, TEN, X
      character P*8
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(11),TEN   )
C     !DASH
      external HI, BYE
C
      data HNDRD /1.D2/
C
      call HI ('NEBO')
C     !BEG
      if(X.lt.TEN) then
        write (P,100) X
  100   format(F8.5)
      else if(X.lt.HNDRD) then
        write (P,101) X
  101   format(F8.4)
      else
        write (P,102) X
  102   format(F8.3)
      end if
C     !END
      call BYE ('NEBO')
C
      return
      end
