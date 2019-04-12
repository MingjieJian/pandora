      subroutine REANY
     $(LUMR,Z,XRK,XRL)
C
C     Rudolf Loeser, 1998 Jun 26
C---- Puts charge exchange data into restart file.
C     !DASH
      save
C     !DASH
      real*8 XRK, XRL, Z
      integer J, LUMR, MCXK, MODE, N
      character LAB*13
C     !COM
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
      equivalence (LEST(55),MCXK )
C
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C---- XINGU       as of 1999 Sep 21
      real*8      AXED,BXED,RCHX,DELCHX
      character   NAMXED*3
      integer     NXI,NPQLM,NPQMX
      parameter   (NXI=10)
C     (Remember to change all users when changing NXI)
      parameter   (NPQLM=15)
C     (Maximum permitted value of principal quantum number n)
C     (NPQLM must not exceed LIMDAT(1) [in popdata.inc], the
C     number of levels in the Hydrogen population ion model.)
      dimension   AXED(NXI), BXED(NXI), NAMXED(NXI)
      dimension   RCHX(NPQLM,NPQLM), DELCHX(NPQLM,NPQLM)
      common      /XINGU1/ AXED,BXED,RCHX,DELCHX
      common      /XINGU2/ NAMXED
      common      /XINGU3/ NPQMX
C---- Charge Exchange data tables
C     .
C     !DASH
      external BUNT, PUNT, YARDEN, HI, BYE
C
C               Z(N), XRK(N,NPQLM), XRL(N,NPQLM)
      dimension Z(*), XRK(N,*),     XRL(N,*)
C
      data MODE /1/
C     !EJECT
C
      call HI ('REANY')
C     !BEG
      call YARDEN (LUMR,1,'CHARGE EXCHANGE')
C
      write (LUMR,100) N
  100 format('N (',I4,' ) > ')
      call BUNT   (LUMR,Z,'Z')
      write (LUMR,101) HEAD
  101 format(A80)
C
      do 103 J = 4,NPQMX
        write (LAB,102) J
  102   format(7X,I3,3X)
        call PUNT (LUMR,XRK(1,J),N,MODE,'XRKH'//LAB)
        call PUNT (LUMR,XRL(1,J),N,MODE,'XRLH'//LAB)
  103 continue
C
      call YARDEN (LUMR,1,'CHARGE EXCHANGE')
C     !END
      call BYE ('REANY')
C
      return
      end
