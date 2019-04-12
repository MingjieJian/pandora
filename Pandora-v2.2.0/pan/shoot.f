      subroutine SHOOT
     $(HEAD,N,Z,XKPC,TAU,SCAT,XJNU,B,S,FD,BHS,YDAMP,IIFLAG,NO,F,TE,ITS,
     $ CNXP,FRS,P,INCDNT,JNUMTH)
C
C     Rudolf Loeser, 1980 Mar 13
C---- Prints Continuum Data.
C     (This is version 3 of SHOOT.)
C     !DASH
      save
C     !DASH
      real*8 B, BHS, CNXP, F, FD, FRS, P, S, SCAT, TAU, TE, XJNU, XKPC,
     $       YDAMP, Z
      integer IIFLAG, IQACP, ITS, JNUMTH, N, NO
      logical INCDNT, SHORT
      character HEAD*12
C     !COM
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(254),IQACP)
C     !DASH
      external TRILL, QUILL, RILL, PANON, LINER, VECOUT, HI, BYE
C
C               SCAT(N), XKPC(N), Z(N), TAU(N), TE(N), BHS(N), CNXP(N),
      dimension SCAT(*), XKPC(*), Z(*), TAU(*), TE(*), BHS(*), CNXP(*),
C
C               XJNU(N), S(N), FD(N), B(N), FRS(N), P(N)
     $          XJNU(*), S(*), FD(*), B(*), FRS(*), P(*)
C     !EJECT
C
      call HI ('SHOOT')
C     !BEG
      if(NO.gt.0) then
        SHORT = IQACP.gt.0
C
C----   Header
        call TRILL    (NO, HEAD, INCDNT, JNUMTH, SHORT)
C
        if(SHORT) then
C----     Abbreviated tables
C
          call VECOUT (NO, TAU,  N, 'Optical Depth'            )
          call VECOUT (NO, S,    N, 'Continuum Source Function')
          call VECOUT (NO, XJNU, N, 'Mean Intensity'           )
        else
C
C----     Full tables
          call QUILL  (NO, IIFLAG, INCDNT, N, SCAT, XKPC, Z, TAU, XJNU,
     $                 S, BHS, B, TE, FD, CNXP)
        end if
C
C----   Trailer
        call RILL     (NO, YDAMP, F, ITS)
C
        if(.not.SHORT) then
C----     Auxiliary printout
          call PANON  (FRS, XJNU, P, 'Jnu', NO)
        end if
      end if
C     !END
      call BYE ('SHOOT')
C
      return
      end
