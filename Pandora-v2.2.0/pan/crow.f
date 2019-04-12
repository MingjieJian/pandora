      subroutine CROW
     $(N,ITMX,INP,A,TITLE,NO,KIBI,IGRF,TRND,PRAT)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Does an iterative summary.
C     INP = 1 means: input set included;
C         = 0 means: input set not included.
C     (This is version 2 of CROW.)
C     !DASH
      save
C     !DASH
      real*8 A, PRAT
      integer IGRF, INP, ITMX, KIBI, MODE, N, NO
      logical GRAF, PRNT, TRND, USE
      character TITLE*15
C     !DASH
      external SUGAR, GRAMA, SLENDER, SPICY, JAY, IMOLA, IDATH, HI, BYE
C
C               A(N,ITMX), PRAT(N)
      dimension A(*),      PRAT(*)
C
      data      MODE /1/
C
      call HI ('CROW')
C     !BEG
C---- Normalize set
      call SUGAR     (N,ITMX,A)
C---- Examine set
      call GRAMA     (N,ITMX,A,USE)
C---- Decide which output format to use
      call IMOLA     (IGRF,USE,PRNT,GRAF)
      if(PRNT) then
C----   Print header
        call SLENDER (NO,TITLE)
C----   Print summary
        call SPICY   (NO,N,ITMX,INP,A)
      end if
      if(GRAF) then
        call IDATH   (NO,N,A,PRAT,ITMX,INP,TITLE,MODE)
      end if
      if(TRND.and.(ITMX.gt.(2+INP))) then
C----   Iterative Trend summary
        call JAY     (N,ITMX,A,TITLE,KIBI)
      end if
C     !END
      call BYE ('CROW')
C
      return
      end
