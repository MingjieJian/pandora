      subroutine ATTIS
     $(X,IX,W,IW,WAVES,KTYPE,IADRS,NW,XCBL,XLB1,XPBL,XLYB)
C
C     Rudolf Loeser, 2005 Mar 23
C---- Supervises PRD-related Background calculations: Part 1
C
C     XCBL is the buffer for Continuum Data Blocks;
C     XPBL is the buffer for Population Data Blocks;
C     XLYB is the H Ly lines data block;
C     XLB1 is the buffer for the Line Intensity Data Block, Part 1.
C
C     (See also "DEMETER" and "CYBELE".)
C     !DASH
      save
C     !DASH
      real*8 TIN, TOTIME, W, WAVES, X, XCBL, XLB1, XLYB, XPBL
      integer I, IADRS, IW, IX, KABS, KEMIT, KHED, KODE, KTRU, KTYPE,
     $        MMDL, MODE, NW, jummy
      logical MOVING, PPRNT, lummy
C     !COM
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(59),MMDL )
C     !DASH
C     !EJECT
      external SECOND, JITNEY, DION, POPIO, BOHOL, FRIGG, FREY, HOOKER,
     $         HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XCBL(Miklen), XPBL(Lenpbl), XLYB(Lenlyb), XLB1(Li1len),
      dimension XCBL(*),      XPBL(*),      XLYB(*),      XLB1(*),
C
C               WAVES(NW), IADRS(NW), KTYPE(NW)
     $          WAVES(*),  IADRS(*),  KTYPE(*)
C
      data KODE,MODE,KTRU  /3, 0, 0/
      data KABS,KEMIT,KHED /1, 1, 1/
C
      call HI ('ATTIS')
C     !BEG
C     (Initialize; JITNEY also sets INIHLL for Lyman Lines opacity)
      call JITNEY   (MOVING, lummy, lummy, jummy)
      call POPIO    ('INIT', jummy, XPBL)
C
      do 100 I = 1,NW
        call SECOND (TIN)
        call HOOKER (I, NW, XLB1(MMDL), PPRNT)
C
C----   Read data into XCBL, amd initialize switches and print controls
        call FREY   (XCBL, X, WAVES(I), IADRS(I), KTYPE(I), KTRU,
     $               MODE, KHED, PPRNT, 'ATTIS')
C
C----   Absorption and emission
        call FRIGG  (X, W, IW, XPBL, XCBL, XLYB, KABS, KEMIT, KODE,
     $               KTRU)
C
C----   Write data
        call BOHOL  (XCBL, MIKLEN, IADRS(I))
C
C----   Output
        call DION   (X, W, IW, XCBL, MOVING, TIN, TOTIME)
  100 continue
C     !END
      call BYE ('ATTIS')
C
      return
      end
