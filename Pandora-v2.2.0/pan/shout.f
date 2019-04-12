      subroutine SHOUT
     $(LU,KODE,NAME)
C
C     Rudolf Loeser, 2004 Apr 20
C---- Prints an alert.
C     !DASH
      save
C     !DASH
      integer KODE, LU
      character NAME*(*)
C     !DASH
      external LINER, DASHER, HI, BYE
C
      call HI ('SHOUT')
C     !BEG
      if((LU.gt.0).and.(KODE.ne.1)) then
C
        call LINER  (2, LU)
        call DASHER (LU)
        write (LU,100) NAME
  100   format(' ','The ion-of-the-run and the built-in background ',
     $             'line contributor data for ',A,' do not all agree.')
        call DASHER (LU)
        call LINER  (2, LU)
C
      end if
C     !END
      call BYE ('SHOUT')
C
      return
      end
