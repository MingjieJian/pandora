      subroutine RODA
     $(N,DMP1,CDW,DW,CNDT,ANT,OMD,PD,UNT,T,FNDT,DEL,IDDL)
C
C     Rudolf Loeser, 2004 May 07
C---- Computes intermediates for BOTTOM.
C     (This is version 2 of RODA.)
C     !DASH
      save
C     !DASH
      real*8 ANT, CDW, CNDT, DEL, DW, FNDT, OMD, PD, T, UNT
      integer IDDL, IQINC, N
      logical DMP1
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
      equivalence (IQQ( 51),IQINC)
C     !DASH
C     !EJECT
      external MOVE1, ZERO1, ARRMUL, ARRSUB, DEAL, KROKANT, KEGLER,
     $         ONE1, HI, BYE
C
C               DW(N), CNDT(N), FNDT(N), ANT(N), OMD(N), UNT(N), PD(N),
      dimension DW(*), CNDT(*), FNDT(*), ANT(*), OMD(N), UNT(*), PD(*),
C
C               DEL(N), T(N)
     $          DEL(*), T(*)
C
      call HI ('RODA')
C     !BEG
C---- Compute T
      call DEAL     (N, CDW, DW, T)
C---- Compute UNT
      call MOVE1    (ANT, N, UNT)
      call ARRMUL   (UNT, T, UNT, N)
C---- Set up FNDT, the incident radiation term
      if(IQINC.le.0) then
        call ZERO1  (FNDT, N)
      else
        call MOVE1  (PD, N, FNDT)
        call ARRMUL (FNDT, T, FNDT, N)
        call ARRMUL (FNDT, CNDT, FNDT, N)
      end if
C---- Compute DEL
      call ONE1     (DEL, N)
      call ARRSUB   (DEL, OMD, DEL, N)
C---- Compute IDDL, the full-solution-limit index
      call KROKANT  (N, OMD, IDDL)
C
      if(DMP1) then
        call KEGLER (N, T, ANT, UNT, CNDT, PD, FNDT, OMD, DEL)
      end if
C     !END
      call BYE ('RODA')
C
      return
      end
