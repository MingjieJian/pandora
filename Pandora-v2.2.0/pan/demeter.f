      subroutine DEMETER
     $(X,IX,W,IW,WAVES,KTYPE,IADRS,NW,XCBL,XLB1,XLB2,ILFLX,MPROM)
C
C     Rudolf Loeser, 2005 Mar 28
C---- Supervises PRD-related Background calculations: Part 2.
C
C     XCBL - is the buffer for Continuum Data Blocks;
C     XLB1 - is the buffer for the Line Intensity Data Block, Part 1;
C     XLB2 - is the buffer for the Line Intensity Data Block, Part 2.
C
C     See also "CYBELE" and "ATTIS".
C     !DASH
      save
C     !DASH
      real*8 TIN, TOTIME, W, WAVES, X, XCBL, XLB1, XLB2
      integer I, IADRS, ILFLX, IW, IX, JNUMTH, KHED, KTRU, KTYPE, MMDL,
     $        MODE, MPROM, NW
      logical INCDNT, MOVING, PPRNT, SPHERE
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
      external CITRINE, AVALON, JITNEY, ROCKET, SECOND, HOOKER, JETLAG,
     $         FREY, SHAKE, BOHOL, POPIO, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB1(Li1len), XLB2(Li2len), XCBL(Miklen), KTYPE(NW),
      dimension XLB1(*),      XLB2(*),      XCBL(*),      KTYPE(*),
C
C               WAVES(NW), IADRS(NW)
     $          WAVES(*),  IADRS(*)
C
      data KHED,MODE,KTRU /1, 1, 0/
C
      call HI ('DEMETER')
C     !BEG
C     (Initialize; JITNEY also sets INIHLL for Lyman Lines opacity)
      call JITNEY    (MOVING, SPHERE, INCDNT, JNUMTH)
C
      do 100 I = 1,NW
        call SECOND  (TIN)
        call HOOKER  (I, NW, XLB1(MMDL), PPRNT)
C
C----   Read data into XCBL, and initialize switches and print controls
        call FREY    (XCBL, X, WAVES(I), IADRS(I), KTYPE(I), KTRU,
     $                MODE, KHED, PPRNT, 'DEMETER')
C
C----   Modify Part-1 results to include PRD-terms
        call CITRINE (X, IX, W, IW, XCBL, I)
C----   Source function and intensity
        call AVALON  (X, IX, W, IW, ILFLX, MPROM, MOVING, SPHERE,
     $                INCDNT, JNUMTH, XCBL, XLB1, XLB2)
C
C----   Write data
        call BOHOL   (XCBL, MIKLEN, IADRS(I))
C
C----   Output
        call SHAKE   (X, W, IW, XCBL, MOVING, INCDNT, JNUMTH, TIN,
     $                TOTIME)
C
        call ROCKET  (I, NW, TOTIME, 'Jnu')
  100 continue
C     !END
      call BYE ('DEMETER')
C
      return
      end
