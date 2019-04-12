      subroutine BEER
     $(XCBL,WAVES,INDADR,KTRN,TE,TR,BTE,BTR,BKPC,BBC)
C
C     Rudolf Loeser, 1986 Jul 10
C---- Retrieves and sets up background continuum data for Line Source
C     Function calculations with frequency-dependent background.
C     (This is version 5 of BEER.)
C     !DASH
      save
C     !DASH
      real*8 BBC, BKPC, BTE, BTR, TE, TR, WAVES, XCBL
      integer INDADR, IQCSF, IQCSW, J, KKOPAC, KKSCON, KTRN, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
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
      equivalence (IQQ( 46),IQCSF)
      equivalence (IQQ( 14),IQCSW)
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK( 6),KKOPAC)
      equivalence (KKK(14),KKSCON)
C     !DASH
C     !EJECT
      external LEYTE, MOVE1, PLUNK, PUCCOON, HI, BYE
C
C               XCBL(Miklen), INDADR(KTRN), WAVES(KTRN), BTE(N), TE(N),
      dimension XCBL(*),      INDADR(*),    WAVES(*),    BTE(*), TE(*),
C
C               BKPC(N,KTRN), BBC(N,KTRN), TR(N,NSL), BTR(N)
     $          BKPC(N,*),    BBC(N,*),    TR(*),     BTR(*)
C
      call HI ('BEER')
C     !BEG
      do 100 J = 1,KTRN
        call LEYTE   (XCBL, MIKLEN, INDADR(J))
C
        call MOVE1   (XCBL(KKOPAC), N, BKPC(1,J))
C
        call PLUNK   (N, WAVES(J), TE, BTE)
        if((IQCSF.le.0).and.(IQCSW.gt.0)) then
          call PLUNK (N, WAVES(J), TR, BTR)
        end if
C
        call PUCCOON (N, XCBL(KKSCON), BTE, BTR, BBC(1,J))
  100 continue
C     !END
      call BYE ('BEER')
C
      return
      end
