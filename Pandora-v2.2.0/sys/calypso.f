      subroutine CALYPSO
     $(LUN)
C     Rudolf Loeser, 1996 Jun 03
C---- Flushes the I/O buffer for logical unit LUN.
C     Presumed to be needed for DEC Unix.
C     !DASH
      save
C     !DASH
      integer LUN
C     !DASH
      external flush
C
C     !BEG
      call flush (LUN)
C     !END
C
      return
      end
