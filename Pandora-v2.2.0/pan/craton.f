      subroutine CRATON
     $(X,W,IW,XNK,XND,NO)
C
C     Rudolf Loeser, 1975 Jul 30
C---- Plots number densities.
C     !DASH
      save
C     !DASH
      real*8 W, X, XND, XNK
      integer IN, IPND, IS, IW, IZLOG, JJHND, JJZ, ML, MOX, N, NL, NO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 11),JJHND)
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
C     !EJECT
      external  TOURMAL, MULATOR, JANGLE, BELET, JARUL, WGIVE, HI, BYE
      intrinsic min
C
      dimension X(*), W(*), IW(*)
C
C               XND(N,NL), XNK(N)
      dimension XND(*),    XNK(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IPND  ),(IN( 2),IZLOG )
C
      call HI ('CRATON')
C     !BEG
      if(NO.gt.0) then
C----   Plot level-population-ranks (up to 26 levels)
        call TOURMAL (XND, N, NL, NO, W, IW)
C
C-----  Plot complete number densities
C       (Get, and allocate, W allotment)
        call JANGLE  (IN, IS, MOX, 'CRATON')
C
C----   Plot "ND"/HND
        ML = min(NL,26)
        call BELET   (N, ML, XND, XNK, W(IPND))
        call MULATOR (NO, N, (ML+1), X(JJZ), X(JJHND), W(IPND), W, IW)
C
C----   Plot "ND" straight
        call BELET   (N, NL, XND, XNK, W(IPND))
        call JARUL   (X(JJZ), W(IPND), IMAGE, W(IZLOG), N, (NL+1), NO)
C
C       (Give back W allotment)
        call WGIVE   (W, 'CRATON')
      end if
C     !END
      call BYE ('CRATON')
C
      return
      end
