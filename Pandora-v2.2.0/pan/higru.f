      subroutine HIGRU
     $(VXS,VXI,VM,X,N,CVXS,FR,IQEXA)
C
C     Rudolf Loeser, 1990 Apr 26
C---- Checks "Source function expansion velocity".
C     (This is version 2 of HIGRU.)
C     !DASH
      save
C     !DASH
      real*8 CVXS, FR, VM, VXI, VXS, X
      integer IQEXA, JJFMV, JJHEA, JJHND, JJZ, N
      logical DONE, KILROY
      character LABEL*35
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(219),JJHEA)
      equivalence (IZOQ(225),JJFMV)
C     !DASH
      external KALMIA, MOVE1, TEARO, CRATE, HI, BYE
C
      dimension X(*)
C
C               VXS(N), VXI(N), FR(N), VM(N)
      dimension VXS(*), VXI(*), FR(*), VM(*)
C
      data LABEL /'Source function expansion velocity'/
C
      call HI ('HIGRU')
C     !BEG
      if(IQEXA.le.0) then
C----   Make sure that VXI and CVXS =0
        call CRATE    (VXI, N, CVXS)
      else
C----   Make sure VXI=-VM when VM exists
        call TEARO    (N, VM, VXI, LABEL, .true., DONE)
        if(.not.DONE) then
C----     Make sure VXI is otherwise set up properly
          KILROY = .true.
          call KALMIA (N, CVXS, X(JJZ), X(JJHND), X(JJHEA), FR,
     $                 X(JJFMV), KILROY, LABEL, 0, VXI)
        end if
      end if
C---- Set up initial VXS
      call MOVE1      (VXI, N, VXS)
C     !END
      call BYE ('HIGRU')
C
      return
      end
