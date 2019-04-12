      subroutine ARETE
     $(X,IX,W,IW,BRNT)
C
C     Rudolf Loeser, 2004 May 26
C---- Computes missing initial values of Bs.
C     (This is version 3 of ARETE.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IBD0, IBD1, IBDD, IBDE, IBDN, IBDR, IBIJ, IIMG, IN, IS,
     $        IW, IWS, IX, JJBDI, JN, METH, MOX, MUX, N, NSL
      logical BRNT, NONE
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
      equivalence (IZOQ( 44),JJBDI)
C     !DASH
C     !EJECT
      external EMERALD, IMMAKE, SUSAN, IGIVE, CYNX, WGIVE, WILY, ONE1,
     $         HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(7)
      equivalence
     $(IN( 1),IBD0  ),(IN( 2),IBD1  ),(IN( 3),IBDN  ),(IN( 4),IBDD  ),
     $(IN( 5),IBDR  ),(IN( 6),IBDE  ),(IN( 7),IBIJ  )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IIMG  )
C
      data METH /1/
C
      call HI ('ARETE')
C     !BEG
      BRNT = .false.
C---- Determine whether any Bs were input
      call SUSAN     (X(JJBDI), NONE)
      if(NONE) then
C
C       (Get, and allocate, W & IW allotments)
        call CYNX    (IN, IS , MOX, 'INGER')
        call IMMAKE  (JN, IWS, MUX, 'INGER')
C
C----   Compute Bs (and save for iterative summary)
        call ONE1    (W(IBIJ), (N*NSL))
        call WILY    (X, IX, W, IW, W(IBIJ), METH, X(JJBDI), IW(IIMG),
     $                W(IBD0), W(IBD1), W(IBDN), W(IBDD), W(IBDR),
     $                W(IBDE))
        call EMERALD (X(JJBDI))
C
        BRNT = .true.
C
C       (Give back W & IW allotments)
        call WGIVE   (W , 'INGER')
        call IGIVE   (IW, 'INGER')
C
      end if
C     !END
      call BYE ('ARETE')
C
      return
      end
