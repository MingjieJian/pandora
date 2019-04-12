      subroutine YARDEN
     $(LU,KODE,TITLE)
C
C     Rudolf Loeser, 1998 Jun 26
C---- Writes markers into a restart file.
C     !DASH
      save
C     !DASH
      integer KODE, LU
      character TITLE*(*)
C     !COM
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C     !DASH
      external HI, BYE
C
      call HI ('YARDEN')
C     !BEG
      if((LU.gt.0).and.((KODE.eq.1).or.(KODE.eq.2))) then
        if(KODE.eq.1) then
          write (LU,100) TITLE, 'start'
  100     format('[ ',A,' data ',A,' ] > ')
          write (LU,101) HEAD
  101     format(A80)
        else
          write (LU,100) TITLE, 'end'
        end if
      end if
C     !END
      call BYE ('YARDEN')
C
      return
      end
