      subroutine EYAK
     $(LU,XCOL,NCL,XLCOA,XLCOB,NCB,YCOL,XLCOD,TE,V,NDW,XCOMX,XMCOA,
     $ LCOW,NECLP,COMU,SRCO,CTCO,CTMX,SHCOP,SHCOC,ZRCO)
C     Rudolf Loeser, 1987 Nov 13
C---- Prints CO-lines opacity parameters.
C     !DASH
      save
C     !DASH
      real*8 COMU, CTCO, CTMX, SHCOC, SHCOP, SRCO, SRT12, SRT13, TE, V,
     $       XCOL, XCOMX, XLCOA, XLCOB, XLCOD, XMCOA, YCOL, ZRCO
      integer IQAVK, IQCOD, IQCOE, LCOW, LU, NCB, NCL, NDW, NECLP
      character TIT*10
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
      equivalence (IQQ(223),IQCOD)
      equivalence (IQQ(303),IQAVK)
      equivalence (IQQ(305),IQCOE)
C     !DASH
      external PADMA, FLOTSAM, ARPAD, LINER, HAKO, CHOTA, VECOUT,
     $         HI, BYE
C
C               XCOL(NCL), TE(N), V(N), XLCOA(NCB), XLCOB(NCB)
      dimension XCOL(*),   TE(*), V(*), XLCOA(*),   XLCOB(*)
C
      call HI ('EYAK')
C     !BEG
      if((LCOW.gt.0).and.(LU.gt.0)) then
C
        call PADMA   (LU,'CO-Lines opacity')
        call FLOTSAM (LU)
        call LINER   (2,LU)
        call ARPAD   (LU,XLCOA,XLCOB,NCB)
        call LINER   (1,LU)
C     !EJECT
        if(NCB.gt.1) then
          write (LU,100) LCOW
  100     format(' ','There are',I6,' lines in these bands.')
        else
          write (LU,101) LCOW
  101     format(' ','There are',I6,' lines in this band.')
        end if
        call HAKO    (YCOL,TIT)
        call LINER   (1,LU)
        write (LU,102) TIT,XCOMX
  102   format(' ','C.S.F. method: ',A,29X,'CO-xmax =',1PE12.4)
        if(NCL.gt.0) then
          call VECOUT (LU,XCOL,NCL,'XCO')
C
          call CHOTA  (TE(NDW),V(NDW),1,SRT12)
          call CHOTA  (TE(NDW),V(NDW),2,SRT13)
          call LINER  (1,LU)
          write (LU,103) NDW,TE(NDW),V(NDW),SRT12,SRT13,XMCOA,COMU,
     $                   SRCO
  103     format(' ','NDW =',I3,5X,'TE(NDW) =',1PE16.8,5X,
     $               'V(NDW) =',E16.8,6X,'SRT(12) =',E16.8,5X,
     $               'SRT(13)=',E16.8/
     $           ' ','Multiplier of van der Waal''s damping ',
     $               'parameter for CO profiles',14X,'MCOA =',E16.8/
     $           ' ','mu-value for opacity calculation, ',
     $               'COMU =',E12.4/
     $           ' ','scattering ratio for opacity calculation, ',
     $               'SRCO =',E12.4)
          call LINER  (1,LU)
          write (LU,104) ZRCO,SHCOP,SHCOC,CTCO,CTMX
  104     format(' ','CO reference height, ZRCO =',1PE12.4/
     $           ' ','CO photospheric scale height, SHCOP =',E12.4/
     $           ' ','CO chromospheric scale height, SHCOC =',E12.4/
     $           ' ','NCO temperature enhancement factor, CTCO =',E12.4/
     $           ' ','Maximum of FCTCO, CTMX =',E12.4)
        end if
C
        if((IQAVK.gt.0).and.(IQCOE.gt.0)) then
          call LINER  (1,LU)
          write (LU,105) NECLP
  105     format(' ','NECLIP =',I4)
        end if
C
        if(IQCOD.gt.0) then
          call LINER  (1,LU)
          write (LU,106) XLCOD
  106     format(' ','Dump wavelength LCOD =',1PE16.8)
        end if
C
      end if
C     !END
      call BYE ('EYAK')
C
      return
      end
