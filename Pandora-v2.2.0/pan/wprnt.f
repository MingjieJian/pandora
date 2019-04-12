      subroutine WPRNT
     $(WSUB,CALLER,INEXT)
C
C     Rudolf Loeser, 1997 Oct 09
C---- Screen messages from "world" management.
C     !DASH
      save
C     !DASH
      integer INEXT
      character CALLER*(*), LINE*100, WSUB*4
C     !DASH
      external HI, BYE
C
      data LINE /' '/
C
      call HI ('WPRNT')
C     !BEG
      write (*,100) LINE(:INEXT), WSUB, CALLER, INEXT
  100 format(' ','xworld: ',A,A4,2X,A,I5)
C     !END
      call BYE ('WPRNT')
C
      return
      end
