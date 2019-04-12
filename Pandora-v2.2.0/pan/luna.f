      subroutine LUNA
     $(X,QELSM,ABD,ELABD)
C
C     Rudolf Loeser, 1991 Jan 02
C---- Sets up elemental abundance as a function of depth.
C     (This is version 2 of LUNA.)
C     !DASH
      save
C     !DASH
      real*8 ABD, ELABD, X
      integer JJHEA, N
      character QELSM*8
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
      equivalence (IZOQ(219),JJHEA)
C     !DASH
      external SET1, ARRMUL, HI, BYE
C
      dimension X(*)
C
C               ELABD(N)
      dimension ELABD(*)
C
      call HI ('LUNA')
C     !BEG
      call SET1     (ELABD,N,ABD)
      if(QELSM(1:2).eq.'HE') then
        call ARRMUL (ELABD,X(JJHEA),ELABD,N)
      end if
C     !END
      call BYE ('LUNA')
C
      return
      end
