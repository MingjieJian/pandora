      subroutine RASHOW
     $(FILDAT,LUN,IOSTAT,NO)
C     Rudolf Loeser, 1986 Jul 02
C---- Print RA file performance data; see remarks in RAFAEL.
C     !DASH
      save
C     !DASH
      real*8 BYTES, FILDAT, FLUN, ZERO
      integer IOSTAT, LUN, NO
C     !DASH
      intrinsic int
C
      dimension FILDAT(11)
C
      data      ZERO /0.D0/
C
C     !BEG
      IOSTAT = 0
      if(NO.gt.0) then
        if(FILDAT(1).gt.ZERO) then
          FLUN = LUN
          if(FILDAT(1).eq.FLUN) then
            BYTES = FILDAT(3)*FILDAT(5)
            write (NO,102)
            write (NO,100) int(FILDAT( 1)),int(FILDAT( 3)),
     $                     int(FILDAT( 2)),int(FILDAT( 5)),BYTES
  100       format(' ','Data for the random access scratch file ',
     $                 'attached to unit #',I4//
     $             ' ','Physical record length =',I11,' bytes; ',
     $                 '(# of next available empty record =',I11,').'//
     $             ' ','Total # of physical records in use =',I11,
     $                 '; file length =',1PE11.4,' bytes.')
            write (NO,102)
            write (NO,101) int(FILDAT( 6)),'written'  ,FILDAT( 7)
            write (NO,102)
            write (NO,101) int(FILDAT( 8)),'rewritten',FILDAT( 9)
            write (NO,102)
            write (NO,101) int(FILDAT(10)),'read'     ,FILDAT(11)
  101       format(' ','A total of',I11,' logical records were ',A,
     $                 '; they contained',1PE11.4,' bytes of data.')
          else
            IOSTAT = -1
          end if
        else
          write (NO,102)
  102     format(' ')
          write (NO,103) LUN
  103     format(/' ','The random access scratch file intended for ',
     $               'unit #',I3,' was never opened.')
        end if
      end if
C     !END
C
      return
      end
