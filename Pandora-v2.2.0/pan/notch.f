      subroutine NOTCH
     $(NO,IO,IT,I,J,W,B)
C
C     Rudolf Loeser, 1981 Feb 12
C---- Prints, for GLUM.
C     (This is version 2 of NOTCH.)
C     !DASH
      save
C     !DASH
      real*8 B, W
      integer I, IO, IT, J, KODE, NO
      logical KILROY
C     !DASH
      external PRIAM, LINER, HI, BYE
C
      data KILROY /.true./
C
      call HI ('NOTCH')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call PRIAM (NO, 'FUDGERS', 7)
        call LINER (1, NO)
        write (NO,100)
  100   format(' ','(This output appears because Rho-fudging was ',
     $             'done [enabled by option RHOFUDGE].)')
C
        call LINER (4,NO)
        write (NO,101)
  101   format(' ',25X,'Final'/
     $         ' ',4X,'IOVER   ISUB',8X,'Weight',14X,'Fudging ',
     $            'at depth   for Trans.',8X,'Weight',12X,'Value')
        call LINER (2,NO)
      end if
C
      KODE = I+J
      if(KODE.eq.0) then
        write (NO,102) IO,IT,W
  102   format(' ',6X,I2,5X,I2,3X,F15.12)
      else if(KODE.gt.0) then
        write (NO,103) IO,IT,I,J,W,B
  103   format(' ',6X,I2,5X,I2,41X,I3,7X,I2,'/1',5X,F15.12,1PE19.9)
      end if
C     !END
      call BYE ('NOTCH')
C
      return
      end
