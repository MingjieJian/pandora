      subroutine ABJECT
     $(NO)
C     Rudolf Loeser, 1995 Oct 25
C---- Ejects to a new page
C     in the file attached to unit "NO".
C     (This is version 2 of ABJECT.)
C     !DASH
      save
C     !DASH
      integer IFF, NO
      logical KILROY
      character FF*1
C     !DASH
      intrinsic char
C
      data KILROY,IFF /.true., '0C'X/
C
C     !BEG
      if(KILROY) then
        FF = char(IFF)
        KILROY = .false.
      end if
      if(NO.gt.0) then
        write (NO,100) FF
  100   format(A1)
      end if
C     !END
C
      return
      end
