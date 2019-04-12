      subroutine SUHARD
     $(LU)
C
C     Rudolf Loeser, 1983 Mar 18
C---- Gets a potential logical unit number.
C     !DASH
      save
C     !DASH
      integer IOVER, JPOP, LU, MO, NO
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
      equivalence (LEST(22),JPOP )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external HI, BYE
C
      call HI ('SUHARD')
C     !BEG
      if(JPOP.gt.0) then
        LU = MO
      else
        if(IOVER.eq.1) then
          LU = NO
        else
          LU = MO
        end if
      end if
C     !END
      call BYE ('SUHARD')
C
      return
      end
