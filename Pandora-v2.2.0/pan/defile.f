      subroutine DEFILE
C
C     Rudolf Loeser, 1984 May 30
C---- Gets rid of unneeded files, in a "Continuum-only" run.
C     !DASH
      save
C     !DASH
      integer I, IFILE, NF
      logical EXIST
C     !DASH
      external HI, BYE
C
      dimension IFILE(5)
C
      data NF /5/
      data IFILE /19, 20, 21, 22, 23/
C
      call HI ('DEFILE')
C     !BEG
      do 100 I = 1,NF
        inquire (unit=IFILE(I), exist=EXIST, err=100)
        if(EXIST) then
          close (unit=IFILE(I), status='DELETE', err=100)
        end if
  100 continue
C     !END
      call BYE ('DEFILE')
C
      return
      end
