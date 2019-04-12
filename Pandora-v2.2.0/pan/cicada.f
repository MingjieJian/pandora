      subroutine CICADA
     $(IU,IL,IST,XNE,FSTKM,TE,V,DW,FRCDL,FMCDL,CRIT,FRAC,ITER,M,MP,N,K,
     $ DRAWM,DDRK,MAX,GOOD)
C
C     Rudolf Loeser, 1992 Apr 09
C---- Prints summary for TABOR.
C     (This is version 3 of CICADA.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, DDRK, DRAWM, DW, FMCDL, FRAC, FRCDL, FSTKM, TE, V,
     $       XNE
      integer IL, IQHST, IST, ITER, IU, IUS, K, M, MAX, MP, N, NO
      logical GOOD, KILROY
      character BLANK*1, SIG*1, STAR*1
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
      equivalence (IQQ(274),IQHST)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external LINER, HI, BYE
C
      data KILROY,IUS /.true., 0/
C     !EJECT
C
      call HI ('CICADA')
C     !BEG
      if(IQHST.gt.0) then
        if(KILROY) then
          KILROY = .false.
          call LINER (5, NO)
          write (NO,100) FSTKM,FRCDL,FMCDL
  100     format(' ','Summary of Stark splitting of Hydrogen lines.',
     $               66X,'(Option HSTSUMM)'//
     $           ' ','For each transition there are LDL values of ',
     $               'DWN (displacement, in wavenumber) and ',
     $               'CDL (relative component strength, normalized)'//
     $           ' ',1PE7.1,1X,'FSTKM, splitting reduction factor'/
     $           ' ',  E7.1,1X,'FRCDL, starting value of "minor ',
     $               'lines" criterion'/
     $           ' ',  E7.1,1X,'FMCDL, maximum value of "minor ',
     $               'lines" criterion')
C
          call LINER (1, NO)
          write (NO,101)
  101     format(' ','DW      Doppler width (in /cm) at depth index ',
     $               'I, for which Ne, Te, and V are listed'/
     $           ' ',8X,'Note: the criterion for truncating the ',
     $               'wings = DW * 10'/
     $           ' ',14X,'the starting value of the "resolvability" ',
     $               'criterion = DW / 10'//
     $           ' ','iter    number of reduction iterations'/
     $           ' ','dDWN    final value of "resolvability" ',
     $               'criterion'/
     $           ' ',8X,'Note: when iter > 0, the distance between ',
     $               'adjacent DWN values is not less than this value'/
     $           ' ','fCDL    final value of "minor components" ',
     $               'criterion'/
     $           ' ',8X,'Note: components whose strength was less ',
     $               'than this fraction of the strongest ',
     $               'were eliminated')
C
          call LINER (1, NO)
          write (NO,102) MAX
  102     format(' ','M       number of theoretical components ',
     $               '(including those at duplicated displacements, ',
     $               'if any)'//
     $           ' ','MP      number of components remaining after ',
     $               'consolidation of duplicated displacements (if ',
     $               'any) and before eliminations'/
     $           ' ','N       number of components remaining after ',
     $               '1) truncation of wings, and'/
     $           ' ',45X,'2) if iter = 0, elimination of "minor ',
     $               'components" using FRCDL'/
     $           ' ','LDL     final remaining number of components, ',
     $               'after reduction iterations (imposed limit ',
     $               'LDLMAX =',I4,')')
C     !EJECT
          call LINER (1, NO)
          write (NO,103)
  103     format(' ',60X,'----- actual -----',10X,'original',
     $               23X,'reduced'/
     $           ' ',' trans',4X,'I',5X,'Ne',8X,'Te',9X,'V',9X,'DW',
     $               5X,'iter',4X,'dDWN',6X,'fCDL',7X,'M',4X,
     $               'max. DWN',4X,'MP',5X,'N',3X,'LDL',4X,'max. DWN')
        end if
C
        if(IUS.ne.IU) then
          IUS = IU
          call LINER (1, NO)
        end if
C
        if(GOOD) then
          SIG = BLANK
        else
          SIG = STAR
        end if
C
        write (NO,104) IU,IL,IST,XNE,TE,V,DW,ITER,CRIT,FRAC,M,DRAWM,
     $                 MP,N,K,SIG,DDRK
  104   format(' ','(',I2,'/',I2,')',I4,1P3E10.2,E11.3,I6,2E10.2,I6,
     $             E12.4,3I6,A1,E11.4)
C
      end if
C     !END
      call BYE ('CICADA')
C
      return
      end
