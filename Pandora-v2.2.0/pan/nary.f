      subroutine NARY
     $(LUMR,Z,CQOUT)
C
C     Rudolf Loeser, 1998 Jun 26
C---- Puts QOUT data into restart file.
C     !DASH
      save
C     !DASH
      real*8 CQOUT, Z
      integer LUMR, N
      character QIONM*8
C     !COM
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (QEST( 1),QIONM)
C     !DASH
      external BUNT, YARDEN, HI, BYE
C
C               Z(N), CQOUT(N)
      dimension Z(*), CQOUT(*)
C
      call HI ('NARY')
C     !BEG
      call YARDEN (LUMR, 1, 'QOUT')
      write (LUMR,100) N
  100 format('N (',I4,' ) > ')
      call BUNT   (LUMR, Z, 'Z')
      write (LUMR,101) HEAD
  101 format(A80)
      write (LUMR,102) QIONM
  102 format('[ For use with ',A,' ] > ')
      call BUNT   (LUMR, CQOUT, 'QIN')
      call YARDEN (LUMR, 2, 'QOUT')
C     !END
      call BYE ('NARY')
C
      return
      end
