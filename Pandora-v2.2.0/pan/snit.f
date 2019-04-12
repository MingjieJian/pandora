      subroutine SNIT
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1973 Apr 04
C---- Drives SUNDRY, to make a Spectrum Summary.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IBRI, ILAM, IMAX, IMOD, IMYX, IN, INT, IS, ISTR, IW, IWS,
     $        IX, IXLT, IY, JJTE, JJZ, JN, LFB, LFBV, MOX, MUX, N, NO
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
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external MIRROR, ZORA, SUNDRY, IGIVE, WGIVE, INLOTH, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IY    ),(IN( 2),ILAM  ),(IN( 3),IBRI  ),(IN( 4),INT   ),
     $(IN( 5),IXLT  )
C
      dimension JN(4)
      equivalence
     $(JN( 1),IMAX  ),(JN( 2),IMYX  ),(JN( 3),ISTR  ),(JN( 4),IMOD  )
C     !EJECT
C
      call HI ('SNIT')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call MIRROR   (IN,IS ,MOX,'SNIT')
      call INLOTH   (JN,IWS,MUX,'SNIT')
C
      call ZORA     (LFBV)
      do 100 LFB = 1,LFBV
        call SUNDRY (W,IW,NO,N,X(JJZ),X(JJTE),IW(IMAX),IW(IMYX),W(IY),
     $               W(ILAM),W(IBRI),W(INT),W(IXLT),IW(ISTR),IW(IMOD),
     $               LFB,LFBV)
  100 continue
C
C     (Give back W & IW allotments)
      call WGIVE    (W ,'SNIT')
      call IGIVE    (IW,'SNIT')
C     !END
      call BYE ('SNIT')
C
      return
      end
