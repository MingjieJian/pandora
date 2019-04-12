      subroutine KIDAL
     $(N,LL,A,Z,ZL,LISTXL,NO,TITLE)
C
C     Rudolf Loeser, 1990 Dec 03
C---- Plots a data array of the upper-level charge exchange calculation.
C     !DASH
      save
C     !DASH
      real*8 A, ONE, Z, ZL
      integer IBEG, IEND, J, LISTXL, LL, N, NO
      character TIT*10, TITLE*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
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
      external ZEBRA, SHRIMP, ABJECT, LINER, KPRINT, HI, BYE
C
C               A(N,LL), Z(N), ZL(N), LISTXL(LL)
      dimension A(*),    Z(*), ZL(*), LISTXL(*)
C
      call HI ('KIDAL')
C     !BEG
C---- Initialize plot image
      call ZEBRA  (Z, ZL, N, LL, IBEG, IEND, A, ZZSMALL, ONE, IMAGE,
     $             TIT, 'KIDAL')
C
C---- Enter points into image
      call SHRIMP (ZL, N, IBEG, IEND, A, LL, ALPHS, 26, ZZSMALL, 2,
     $             IMAGE)
C
C---- Write header and image
      call ABJECT (NO)
      write (NO,100) TITLE,TIT
  100 format(' ','Graph of ',A,' vs. ',A10)
      call LINER  (1, NO)
      call KPRINT (IMAGE, NO)
C
C---- Write legend
      call LINER  (1, NO)
      write (NO,101) (ALPHS(J),LISTXL(J),J=1,LL)
  101 format(' ',A,' = Level ',I3,6X,A10,I3,6X,A10,I3,6X,A10,I3,6X,
     $                               A10,I3,6X,A10,I3,6X,A10,I3)
C     !END
      call BYE ('KIDAL')
C
      return
      end
