      subroutine RAREY
     $(LUMR,XK,XJBN)
C
C     Rudolf Loeser, 1998 Jun 26
C---- Puts Lyman-Jbar data into restart file.
C     !DASH
      save
C     !DASH
      real*8 XJBN, XK
      integer KK, LUMR, MODE, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(10),KK )
C     !DASH
      external PUNT, PANT, YARDEN, HI, BYE
C
C               XK(KK), XJBN(N,KK)
      dimension XK(*),  XJBN(*)
C
      data MODE /1/
C
      call HI ('RAREY')
C     !BEG
      call YARDEN (LUMR,1,'LYMAN-JB')
C
      write (LUMR,100) N,KK
  100 format('N (',I4,' )  KK (',I4,' ) > ')
      call PUNT   (LUMR,XK,KK,MODE,'XK')
      call PANT   (LUMR,XJBN,N,KK,MODE,'JB')
C
      call YARDEN (LUMR,2,'LYMAN-JB')
C     !END
      call BYE ('RAREY')
C
      return
      end
