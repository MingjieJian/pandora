      subroutine HIM
     $(I,N,NL,MSL,X,GM,XND,NOK,BD)
C
C     Rudolf Loeser, 1974 Mar 20
C---- Gets other parameters at depth I, for HAMMER.
C     !DASH
      save
C     !DASH
      real*8 BD, GM, X, XND
      integer I, JJBDI, JJGM, JJXND, MSL, N, NL
      logical NOK
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 16),JJGM )
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 44),JJBDI)
C     !DASH
      external HIP, HI, BYE
C
      dimension X(*)
C
C               GM(NL), XND(NL), BD(NL)
      dimension GM(*),  XND(*),  BD(*)
C
      call HI ('HIM')
C     !BEG
      call HIP (I, N, NL, MSL, X(JJGM), GM, X(JJXND), XND, NOK,
     $          X(JJBDI), BD)
C     !END
      call BYE ('HIM')
C
      return
      end
