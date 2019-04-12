      subroutine FORAGER
     $(NO)
C
C     Rudolf Loeser, 1995 May 22
C---- Prints the list of background contributors.
C     (This is version 4 of FORAGER.)
C     !DASH
      save
C     !DASH
      integer I, IDR, INA, INR, JDR, JNA, JNR, JTE, KO, LTE, NO
      character NOTES*10, ONOFF*3, SLTE*1, TYPE*10, YESNO*3
C     !COM
C---- OPACITY     as of 2007 Jan 12
C     Paraphernalia for background absorption/emission contributors.
C
C     (Must recompile BARE, BRACE, FORAGER & SHARI when changing NABS!)
      parameter   (NABS=45)
C
      integer     NABS,NOPAC,KOPAC,TOPAC,LOPAC
      character   CNAME*24,SYMID*1,SHNAM*6
      dimension   KOPAC(NABS),LOPAC(NABS),SYMID(NABS)
      dimension   CNAME(NABS),SHNAM(NABS)
C
      common      /OPAC1/ NOPAC
      common      /OPAC2/ KOPAC
      common      /OPAC3/ LOPAC
      common      /OPAC4/ CNAME
      common      /OPAC5/ SYMID
      common      /OPAC6/ SHNAM
C
C     NOPAC = number of contributors
C     KOPAC = contributor status switch: 0 = don't use, 1 = use
C     CNAME = name (description) of contributor
C             NOTE: This sequence of names establishes the indices or
C                   ordinals by which the contributors are also known.
C     SHNAM = "short" name of contributor
C     LOPAC = "printout order" list of contributor ordinals
C     SYMID = scratch space for symbolic identifiers
C     .
C     !DASH
C     !EJECT
      external  LINER, SHIM, HI, BYE
      intrinsic min, max
C
      dimension INR(NABS), IDR(NABS), INA(NABS), YESNO(3), NOTES(NABS),
     $          LTE(NABS), TYPE(3), ONOFF(2), SLTE(2)
C
      data ONOFF /'off', ' on'/
      data YESNO /' no', 'yes', ' '/
      data SLTE  /' '  , '*'  /
      data TYPE  /'absorption', 'scattering', ' '/
C
      data NOTES /
     $ ' '    , ' '    , ' '    , '2'    , ' '    ,
     $ ' '    , ' '    , ' '    , ' '    , ' '    ,
     $ '2,4'  , ' '    , ' '    , '6'    , '6'    ,
     $ '2,4'  , ' '    , ' '    , ' '    , ' '    ,
     $ ' '    , '1,3,7', '1,3,7', '1,3,8', '1,3,8',
     $ ' '    , '5'    , '10'   , ' '    , '5'    ,
     $ '1,3,9', '1,3,9', '4'    , '4'    , '4'    ,
     $ '4'    , ' '    , ' '    , '4'    , ' '    ,
     $ ' '    , ' '    , ' '    , '4'    , '4'    /
C
      data      INR /
     $ 0, 0, 1, 1, 0,  0, 0, 0, 0, 0,  0, 0, 0, 1, 0,  1, 0, 0, 0, 0,
     $ 0, 0, 1, 0, 1,  0, 0, 1, 1, 1,  0, 1, 0, 0, 0,  1, 0, 0, 0, 0,
     $ 0, 0, 0, 0, 0/
C
      data      IDR /
     $ 1, 1, 0, 0, 1,  1, 1, 1, 1, 1,  1, 1, 1, 0, 1,  0, 1, 1, 1, 1,
     $ 1, 1, 0, 1, 0,  1, 1, 0, 0, 0,  1, 0, 1, 1, 1,  0, 1, 1, 1, 1,
     $ 1, 1, 1, 1, 1/
C
      data      INA /
     $ 1, 1, 0, 0, 1,  1, 1, 1, 1, 1,  1, 1, 1, 0, 1,  0, 1, 1, 1, 1,
     $ 1, 1, 0, 1, 0,  0, 1, 0, 0, 0,  1, 0, 1, 1, 1,  0, 1, 1, 1, 1,
     $ 1, 1, 1, 1, 1/
C
      data      LTE /
     $ 0, 1, 0, 0, 0,  0, 0, 0, 1, 0,  0, 0, 1, 0, 0,  0, 0, 0, 0, 0,
     $ 0, 1, 0, 1, 0,  0, 1, 0, 0, 0,  1, 0, 0, 0, 0,  0, 0, 0, 0, 1,
     $ 1, 0, 0, 0, 0/
C     !EJECT
C
      call HI ('FORAGER')
C     !BEG
      if(NO.gt.0) then
        write (NO,100)
  100   format(' ',37X,'run-'/
     $         ' ',' short',31X,'wide',24X,'included',3X,'included',3X,
     $             'included'/
     $         ' ','  name',17X,'full name',3X,'status',2X,'index',7X,
     $             'type',6X,'in TSCA?',3X,'in PABS?',3X,'in NBHS?',4X,
     $             'Notes (see below)')
        call LINER  (1, NO)
C
        do 102 I = 1,NABS
          KO  = max((min(KOPAC(I),1)),0)+1
          JNR = INR(I)+1
          JDR = IDR(I)+1
          JNA = INA(I)+1
          JTE = LTE(I)+1
          write (NO,101) SHNAM(I),CNAME(I),ONOFF(KO),I,TYPE(JNR),
     $                   YESNO(JNR),YESNO(JDR),YESNO(JNA),SLTE(JTE),
     $                   NOTES(I)
  101     format(' ',A6,4X,A24,4X,A3,4X,I3,4X,A10,8X,A3,8X,A3,8X,A3,A1,
     $               3X,A10)
          call SHIM (I, 5, NO)
  102   continue
C
        call LINER  (1, NO)
        write (NO,103)
  103   format(' ',95X,'* = LTE')
      end if
C     !END
      call BYE ('FORAGER')
C
      return
      end
