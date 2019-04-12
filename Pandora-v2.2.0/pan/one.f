      subroutine ONE
     $(TS,TAUIJ,NO)
C
C     Rudolf Loeser, 1968 Jul 31
C---- Produces the comparative TAU-scale graph.
C     !DASH
      save
C     !DASH
      real*8 TAUIJ, TS, ZAX, ZIN
      integer M, N, NO, NT
      character LINE*117
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 3),M  )
      equivalence (JZQ( 5),NT )
C
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C     !DASH
      external ABJECT, LINER, TWOO, THREEE, FORE, FIVE, HI, BYE
C
C               TS(M), TAUIJ(N,NT)
      dimension TS(*), TAUIJ(*)
C
      call HI ('ONE')
C     !BEG
C---- Print heading
      call ABJECT (NO)
      write (NO,100)
  100 format(' ','Logarithmic comparison plot of TAU scales.',70X,
     $           '(Option LSCALE)')
      call LINER  (2,NO)
C
C---- Determine graph limits
      call TWOO   (TS,M,TAUIJ,N,NT,ZIN,ZAX)
C---- Print a reference scale
      call THREEE (IMAGE,ZAX,ZIN,NO,LINE)
C---- Construct the comparative graph
      call FORE   (IMAGE,ZIN,ZAX,TS,M,TAUIJ,N,NT)
C---- and print it
      call FIVE   (IMAGE,LINE,NO,N,NT)
C     !END
      call BYE ('ONE')
C
      return
      end
