      subroutine HIPPIAS
     $(N,Z,TE,SIFLX,SIDER,TEFF)
C
C     Rudolf Loeser, 1985 Aug 15
C---- Saves integrated flux quantities in output file, if needed.
C     (This is version 2 of HIPPIAS.)
C     !DASH
      save
C     !DASH
      real*8 SIDER, SIFLX, TE, TEFF, Z
      integer IQFXV, LUMR, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(29),LUMR )
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
      equivalence (IQQ(191),IQFXV)
C     !DASH
      external YARDEN, BUNT, HI, BYE
C
C               Z(N), TE(N), SIFLX(N), SIDER(N), TEFF(N)
      dimension Z(*), TE(*), SIFLX(*), SIDER(*), TEFF(*)
C     !EJECT
C
      call HI ('HIPPIAS')
C     !BEG
      if(IQFXV.gt.0) then
        call YARDEN (LUMR,1,'INTEGRATED FLUX')
C
        write (LUMR,100) N
  100   format('N (',I4,') > ')
C
        call BUNT   (LUMR,Z    ,'Z')
        call BUNT   (LUMR,TE   ,'TE')
        call BUNT   (LUMR,SIFLX,'INT_FLX')
        call BUNT   (LUMR,SIDER,'INT_DER')
        call BUNT   (LUMR,TEFF ,'TEFF')
C
        call YARDEN (LUMR,2,'INTEGRATED FLUX')
      end if
C     !END
      call BYE ('HIPPIAS')
C
      return
      end
