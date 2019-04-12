      subroutine DRYER
     $(LUMR,Z,TR)
C
C     Rudolf Loeser, 1998 Jun 26
C---- Puts TR data into restart file.
C     !DASH
      save
C     !DASH
      real*8 TR, Z
      integer LUMR, MODE, N, NSL
      logical ZTR
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(40),NSL)
C     !DASH
      external BUNT, PANT, NAUGHTD, YARDEN, HI, BYE
C
C               Z(N), TR(N,NSL)
      dimension Z(*), TR(*)
C
      data MODE /1/
C
      call HI ('DRYER')
C     !BEG
      call NAUGHTD  (TR,1,(N*NSL),ZTR)
      if(.not.ZTR) then
        call YARDEN (LUMR,1,'TR')
C
        write (LUMR,100) N,NSL
  100   format('N (',I4,' )  NSL (',I4,' ) > ')
        call BUNT   (LUMR,Z,'Z')
        call PANT   (LUMR,TR,N,NSL,MODE,'TRN')
C
        call YARDEN (LUMR,2,'TR')
      end if
C     !END
      call BYE ('DRYER')
C
      return
      end
