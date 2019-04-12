      subroutine CYBELE
     $(X,IX,W,IW,WAVES,KTYPE,IADRS,NW,XCBL,XPBL,XLYB,KABS,KEMIT,KCSF,
     $ KOUT,KODE,KTRU)
C
C     Rudolf Loeser, 1980 Feb 04
C---- Supervises Background (continuum) Calculations.
C     (This is version 2 of CYBELE.)
C
C---- KODE = 3: current line (PRD, FDB)
C          = 2: Lyman
C          = 1: everything else
C
C---- KTRU = 0: regular background (continuum)
C          = 1: "line-free" background
C
C---- The processing options are controlled by these switches:
C     KABS  - absorbers needed;
C     KEMIT - emitters needed;
C     KCSF  - continuum source function needed; and
C     KOUT  - printout needed (if permitted).
C
C     XCBL is the buffer for Continuum Data Blocks;
C     XPBL is the buffer for Population Data Blocks;
C     XLYB is the H Ly lines data block.
C
C---- See also "DEMETER" and "ATTIS".
C     !DASH
      save
C     !DASH
      real*8 TIN, TOTIME, W, WAVES, X, XCBL, XLYB, XPBL
      integer I, IADRS, IW, IX, JNUMTH, KABS, KCSF, KEMIT, KHED, KODE,
     $        KOUT, KTRU, KTYPE, MODE, NW, jummy
      logical INCDNT, LCON, LCSF, LOUT, LSAV, MOVING, PPRNT, SPHERE
C     !COM
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
C     !DASH
C     !EJECT
      external SECOND, JITNEY, DIMPLE, DEANNA, POPIO, QUICMI, BOHOL,
     $         FRIGG, FREY, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XCBL(Miklen), XPBL(Lenpbl), XLYB(Lenlyb),
      dimension XCBL(*),      XPBL(*),      XLYB(*),
C
C               WAVES(NW), IADRS(NW), KTYPE(NW)
     $          WAVES(*),  IADRS(*),  KTYPE(*)
C
      data KHED,MODE,PPRNT /0, 0, .false./
C
      call HI ('CYBELE')
C     !BEG
C     (Initialize; JITNEY also sets INIHLL for Lyman Lines opacity)
      call JITNEY     (MOVING, SPHERE, INCDNT, JNUMTH)
      call POPIO      ('INIT', jummy, XPBL)
      call QUICMI     (KABS, KEMIT, KCSF, KOUT, LCON, LCSF, LSAV,
     $                 LOUT)
C
      do 100 I = 1,NW
        call SECOND   (TIN)
C
C----   Read data into XCBL, amd initialize switches and print controls
        call FREY     (XCBL, X, WAVES(I), IADRS(I), KTYPE(I), KTRU,
     $                 MODE, KHED, PPRNT, 'CYBELE')
C
        if(LCON) then
C----     Absorption and emission
          call FRIGG  (X, W, IW, XPBL, XCBL, XLYB, KABS, KEMIT, KODE,
     $                 KTRU)
        end if
C
        if(LCSF) then
C----     Source function and intensity
          call DIMPLE (X, W, IW, INCDNT, JNUMTH, XCBL, KODE, KTRU)
        end if
C
        if(LSAV) then
C----     Write data
          call BOHOL  (XCBL, MIKLEN, IADRS(I))
        end if
C
C----   Output
        if(LOUT) then
          call DEANNA (X, W, IW, XCBL, KODE, KTRU, MOVING, SPHERE,
     $                 INCDNT, JNUMTH, TIN, TOTIME)
        end if
  100 continue
C     !END
      call BYE ('CYBELE')
C
      return
      end
