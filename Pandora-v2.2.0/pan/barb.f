      subroutine BARB
     $(IQRK,IQRL,NSL,KOLEV)
C
C     Rudolf Loeser, 1991 Feb 15
C---- Sets up IQRK and IQRL for BRENDA.
C     (This is version 2 of BARB.)
C     !DASH
      save
C     !DASH
      integer IQRK, IQRL, KOLEV, NSL
C     !DASH
      external ZEROI, HI, BYE
C
C               IQRK(NSL), IQRL(NSL)
      dimension IQRK(*),   IQRL(*)
C
      call HI ('BARB')
C     !BEG
      call ZEROI (IQRK,1,NSL)
      call ZEROI (IQRL,1,NSL)
      IQRK(KOLEV) = 1
      IQRL(KOLEV) = 1
C     !END
      call BYE ('BARB')
C
      return
      end
