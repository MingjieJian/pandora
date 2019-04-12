      subroutine SCRAP
     $(LENGTH,IBLOCK)
C     Rudolf Loeser, 1987 Jul 14
C---- Returns
C
C     LENGTH - the physical record length (in bytes), and
C     IBLOCK - the number of bytes per disk block,
C
C     for the PANDORA random-access scratch file.
C     !DASH
      save
C     !DASH
      integer IBLOCK, LENGTH
C
C     !BEG
      IBLOCK = 512
      LENGTH = 6*IBLOCK
C     !END
C
      return
      end
