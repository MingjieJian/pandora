      subroutine DITTANY
     $(LU,X,DW,VM)
C
C     Rudolf Loeser, 1992 Oct 15
C---- Prints Doppler Width for CRUMB.
C     (This is version 2 of DITTANY.)
C     !DASH
      save
C     !DASH
      real*8 DW, VM, X
      integer JJTE, JJV, JJVR, LU, MLSFP, N, NDW, NOTA, NOTX, NT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 5),NT )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 12),JJV  )
      equivalence (IZOQ(165),JJVR )
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
      equivalence (KZQ(  1),NDW  )
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
      equivalence (LEST(43),MLSFP)
      equivalence (LEST(27),NOTA )
      equivalence (LEST(25),NOTX )
C     !DASH
C     !EJECT
      external BOHUN, HI, BYE
C
      dimension X(*)
C
C               VM(N), DW(N,NT)
      dimension VM(*), DW(*)
C
      call HI ('DITTANY')
C     !BEG
      if(LU.gt.0) then
        if((MLSFP.gt.0).and.((NOTA.gt.0).or.(NOTX.gt.0))) then
          call BOHUN (LU, N, NT, NDW, X(JJTE), X(JJV), DW, VM, X(JJVR))
        end if
      end if
C     !END
      call BYE ('DITTANY')
C
      return
      end
