      subroutine PUELLA
     $(X,W,NW,WAVES,XMULT,LTYP,XLTIT,SPHERE,TAUK,SCON,OPAC,FD,CNXP,
     $ WVNUM,WTAB,IJECT,IMG,LINFLX,LINK,XLB3,NO)
C
C     Rudolf Loeser, 1980 Jun 19
C---- Controls Continuum Flux Calculation.
C     (XLB3 is needed only when LINFLX = true.)
C     (This is version 2 of PUELLA.)
C     !DASH
      save
C     !DASH
      real*8 CNXP, FD, OPAC, SCON, TAUK, W, WAVES, WTAB, WVNUM, X, XLB3,
     $       XLTIT, XMULT
      integer ICNXU, IDF, IFDU, IJECT, IMG, IN, IOPAU, IS, ISCNU, ISF,
     $        ITAUU, ITEU, ITF, IZU, JJFRR, JJTE, JJZ, LINK, LTYP, MOX,
     $        MRR, N, NO, NW
      logical LINFLX, SPHERE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(15),MRR)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(135),JJFRR)
      equivalence (IZOQ(  7),JJTE )
C     !DASH
C     !EJECT
      external LEX, STELLA, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               WTAB(Nmkuse), LTYP(Nmkuse), FD(N,Nmkuse), WAVES(Nmkuse),
      dimension WTAB(*),      LTYP(*),      FD(*),        WAVES(*),
C
C               SCON(N,Nmkuse), TAUK(N,Nmkuse), IMG(N), CNXP(N,Nmkuse),
     $          SCON(*),        TAUK(*),        IMG(*), CNXP(*),
C
C               XLTIT(Nmkuse), OPAC(N,Nmkuse), XMULT(Nmkuse),
     $          XLTIT(*),      OPAC(*),        XMULT(*),
C
C               WVNUM(Nmkuse), XLB3(Li3len)
     $          WVNUM(*),      XLB3(*)
C
      dimension IN(10)
      equivalence
     $(IN( 1),ITF   ),(IN( 2),ISF   ),(IN( 3),IDF   ),(IN( 4),ICNXU ),
     $(IN( 5),IZU   ),(IN( 6),ITEU  ),(IN( 7),ITAUU ),(IN( 8),ISCNU ),
     $(IN( 9),IFDU  ),(IN(10),IOPAU )
C
      call HI ('PUELLA')
C     !BEG
C     (Get, and allocate, W allotment)
      call LEX    (IN, IS, MOX, 'PUELLA')
C
      call STELLA (NW, N, MRR, WAVES, XMULT, LTYP, XLTIT, SPHERE,
     $             W(ITF), W(ISF), W(IDF), X(JJTE), X(JJZ), X(JJFRR),
     $             IJECT, TAUK, SCON, OPAC, FD, CNXP, W(IZU), W(ITEU),
     $             W(ITAUU), W(ISCNU), W(IFDU), W(IOPAU), W(ICNXU),
     $             WVNUM, WTAB, LINFLX, LINK, XLB3, IMG, W, NO)
C
C     (Give back W allotment)
      call WGIVE  (W, 'PUELLA')
C     !END
      call BYE ('PUELLA')
C
      return
      end
