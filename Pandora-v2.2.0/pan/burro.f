      subroutine BURRO
     $(X,RLI,IQRL,LU)
C
C     Rudolf Loeser, 1991 Feb 15
C---- Drives calculation of additional photoionization.
C     (This is version 2 of BURRO.)
C     !DASH
      save
C     !DASH
      real*8 RLI, X
      integer IQRL, JJYK, LU, N, NSL
      logical ZYK
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(40),NSL)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 72),JJYK )
C     !DASH
      external NAUGHTD, YETI, HI, BYE
C
      dimension X(*)
C
C               RLI(N,NSL), IQRL(NSL)
      dimension RLI(*),     IQRL(*)
C
      call HI ('BURRO')
C     !BEG
      call NAUGHTD (X(JJYK),1,NSL,ZYK)
      if(.not.ZYK) then
        call YETI  (N,NSL,X(JJYK),RLI,IQRL,LU)
      end if
C     !END
      call BYE ('BURRO')
C
      return
      end
