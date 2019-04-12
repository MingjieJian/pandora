      subroutine DRAYNE
     $(LUMR,Z,HE304)
C
C     Rudolf Loeser, 1998 Jun 26
C---- Puts HE304 into restart file.
C     !DASH
      save
C     !DASH
      real*8 HE304, Z
      integer LUMR, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C     !DASH
      external BUNT, YARDEN, HI, BYE
C
C               Z(N), HE304(N)
      dimension Z(*), HE304(*)
C
      call HI ('DRAYNE')
C     !BEG
      call YARDEN (LUMR,1,'HE304')
C
      write (LUMR,100) N
  100 format('N (',I4,' ) >')
      call BUNT   (LUMR,Z    ,'Z')
      write (LUMR,101) HEAD
  101 format(A80)
      call BUNT   (LUMR,HE304,'HE304')
C
      call YARDEN (LUMR,2,'HE304')
C     !END
      call BYE ('DRAYNE')
C
      return
      end
