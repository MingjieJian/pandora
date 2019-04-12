      subroutine OPTITOR
     $(QOPTNAM,LOOK)
C
C     Rudolf Loeser, 1991 Nov 21
C---- Checks for obsolete and deleted OPTION names.
C     (This is version 2 of OPTITOR.)
C     !DASH
      save
C     !DASH
      integer KIND, LOOK, LOOKD, LOOKF, LUEO, NOLD, NOUT
      logical KILROY
      character CURRNT*8, DELETE*8, FORMER*8, OPTN*8, QOPTNAM*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      parameter (NOLD=66, NOUT=8)
      dimension FORMER(NOLD), CURRNT(NOLD), DELETE(NOUT)
C
      data DELETE /
     $ 'PERPASS ', 'GNVRS   ', 'NORMSW  ', 'RIJONLY ', 'NRHOW   ',
     $ 'ABRWAVE ', 'COMPRHO ', 'DRXI    '/
C
      data FORMER /
     $ 'ABSUM   ', 'APOFILE ', 'CDRPRNT ', 'CKGRAPH ', 'CKSPRNT ',
     $ 'CONDUST ', 'CONPUN  ', 'CPA     ', 'CPCOMP  ', 'CPLINE  ',
     $ 'CSFDP   ', 'CSGRAPH ', 'D2DUMP  ', 'DEFLUX  ', 'DETAUPR ',
     $ 'DPDWOUT ', 'DRDUMP  ', 'DRFDUMP ', 'ECLIPUN ', 'ELECOUT ',
     $ 'EMITGR  ', 'EMITOUT ', 'EPDPRNT ', 'FBDOUT  ', 'FDBDUMP ',
     $ 'FELECD  ', 'FELECP  ', 'GDSP    ', 'HNPRINT ', 'INGRAPH ',
     $ 'ITDUMP  ', 'JNUDMP  ', 'JNUPRINT', 'LYMCP   ', 'LYMDP   ',
     $ 'METPR   ', 'NCPRNT  ', 'NPLOT   ', 'NUMBER  ', 'OPACGR  ',
     $ 'OPACOUT ', 'OUTSET  ', 'PDRATI  ', 'PERDUMP0', 'PERDUMP1',
     $ 'PERDUMP2', 'PERDUMP3', 'PRAPI   ', 'PRAPIC  ', 'PRBDET  ',
     $ 'PRCONPR ', 'PRECOMD ', 'PRFDBCR ', 'PRNTBBRD', 'PROPUN  ',
     $ 'RATEGR  ', 'RHBPRINT', 'SEPRINT ', 'SNDBDP  ', 'SOBDUMP ',
     $ 'STANOUT ', 'STIMOUT ', 'TAUFAR  ', 'TNUOUT  ', 'WNDDUMP ',
     $ 'CHXCNG  '/
C     !EJECT
      data CURRNT /
     $ 'OPASUM  ', 'SPECSAV ', 'RATECOPR', 'CHKGRAF ', 'KSHLCOPR',
     $ 'DUSTCOPR', 'CONSAV  ', 'ADDCOPR ', 'COMPCOPR', 'LINECOPR',
     $ 'CSFDMP  ', 'CSFGRAF ', 'DUSTDMP ', 'FLUXDMP ', 'TAUDMP  ',
     $ 'DPDWPRNT', 'DRDMP   ', 'DRFDMP  ', 'ECLISAV ', 'ELECPRNT',
     $ 'EMIGRAF ', 'EMIPRNT ', 'EPDMP   ', 'BDPRNT  ', 'FDBDMP  ',
     $ 'FELEDMP ', 'FELEPRNT', 'GDSDMP  ', 'HNPRNT  ', 'INTGRAF ',
     $ 'ITDMP   ', 'ITERJNU ', 'JNUPRNT ', 'LYMCOPR ', 'LYMDMP  ',
     $ 'METPRNT ', 'PRDPRNT ', 'POPGRAF ', 'POPPRNT ', 'OPAGRAF ',
     $ 'OPAPRNT ', 'RATEPRNT', 'RATEFULL', 'PERDMP0 ', 'PERDMP1 ',
     $ 'PERDMP2 ', 'PERDMP3 ', 'APHIPRNT', 'APHICOPR', 'BDMP    ',
     $ 'PRDCOPR ', 'RCOMPRNT', 'FDBCOPR ', 'BDMP    ', 'PROSAV  ',
     $ 'RATEGRAF', 'RHBPRNT ', 'SEPRNT  ', 'NBPRNT  ', 'SOBDMP  ',
     $ 'STANPRNT', 'STIMPRNT', 'TAUPRNT ', 'PRODMP  ', 'WNDMP   ',
     $ 'CHEXUP  '/
C     !DASH
      external LOOKUC, MUSHED, MASHED, HI, BYE
C
C
      call HI ('OPTITOR')
C     !BEG
      LOOK   = 0
      OPTN   = QOPTNAM
      KILROY = .true.
C
      call LOOKUC   (FORMER, NOLD, OPTN, KIND, LOOKF)
      if(LOOKF.eq.1) then
C----   This is an obsolete former option name -- replacement exists
        LOOK = 1
        call MUSHED ('OPTITOR', 3, KILROY)
        write (LUEO,100) QOPTNAM,CURRNT(KIND)
  100   format(' ','The option name ',A,' has been replaced - ',
     $             'please remove from input.'/
     $         ' ','The current option ',A,' is the closest ',
     $             'equivalent.')
      end if
C
      call LOOKUC   (DELETE, NOUT, OPTN, KIND, LOOKD)
      if(LOOKD.eq.1) then
C----   This former option does not exist anymore
        LOOK = 1
        call MUSHED ('OPTITOR', 3, KILROY)
        write (LUEO,101) QOPTNAM
  101   format(' ','The former option ',A,' no longer exists - ',
     $             'please remove from input.')
      end if
C
      if(.not.KILROY) then
        call MASHED ('OPTITOR')
      end if
C     !END
      call BYE ('OPTITOR')
C
      return
      end
