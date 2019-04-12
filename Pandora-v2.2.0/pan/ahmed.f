      subroutine AHMED
     $(X,W,IW)
C
C     Rudolf Loeser, 2003 Nov 07
C---- Computes initial Z-table from input mass.
C     !DASH
      save
C     !DASH
      real*8 W, X, YH
      integer IFF, IGG, IIMG, IN, IS, IW, IWS, IXCBL, JJDGM, JJHND,
     $        JJMSI, JJPMG, JJTE, JJVT, JJXNE, JJZ, JN, KMASN, KZXST,
     $        MOX, MUX, N, NO
      logical lummy
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 33),JJMSI)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 49),JJVT )
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(151),JJDGM)
      equivalence (IZOQ(265),JJPMG)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(  9),JJXNE)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 15),YH   )
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
      equivalence (LEST(68),KZXST)
      equivalence (LEST(30),KMASN)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external SULTAN, IMMAKE, SARCASM, PRIAM, PARODY, WGIVE, IGIVE,
     $         HI, BYE
C
      dimension X(*), W(*), IW(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IFF   ),(IN( 2),IGG   ),(IN( 3),IXCBL )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IIMG  )
C
      call HI ('AHMED')
C     !BEG
      if((KZXST.le.0).and.(KMASN.gt.0)) then
C       (Get, and allocate, W & IW allotments)
        call SULTAN  (IN, IS , MOX, 'AHMED')
        call IMMAKE  (JN, IWS, MUX, 'AHMED')
C
C----   Write output header
        call PRIAM   (NO, 'Z SCALE', 7)
C----   Compute G (a la HSE)
        call SARCASM (N, YH, X(JJTE), X(JJHND), X(JJXNE), X(JJVT),
     $                X(JJPMG), X(JJDGM), W(IFF), W(IGG), W)
C----   Compute Z
        call PARODY  (X, W, IW, X(JJMSI), W(IGG), X(JJZ), N, W(IXCBL),
     $                IW(IIMG), lummy, 2)
        KZXST = 1
C
C       (Give back W & IW allotments)
        call WGIVE   (W , 'AHMED')
        call IGIVE   (IW, 'AHMED')
      end if
C     !END
      call BYE ('AHMED')
C
      return
      end
