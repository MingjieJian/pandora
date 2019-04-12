      subroutine CONGO
     $(SQ,SM,EP1,SC,G,EP2)
C
C     Rudolf Loeser, 1987 Nov 06
C---- Prints, for EUROPE.
C     !DASH
      save
C     !DASH
      real*8 EP1, EP2, G, SC, SM, SQ
      integer LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('CONGO')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100) SQ,SM,EP1,SC,G,EP2
  100 format(' ','SQ=',1PE12.4,2X,'SM=',E12.4,2X,'EP1=',E12.4,
     $            2X,'SC=',E12.4,2X,'GNV1=',E12.4,2X,'EP2=',E12.4)
C     !END
      call BYE ('CONGO')
C
      return
      end
