      subroutine KOHL
     $(IPZER,WNJNK,NERM,EPTAU,IQPGA,IQSRJ,IOMX,MS,NS,NL,ION,ASMCR,
     $ NIASM,IXASM,MSEDG,MSEDW,NO)
C
C     Rudolf Loeser, 1988 Jun 28
C---- Prints various control data, for DABBLE.
C     (This is version 2 of KOHL.)
C     !DASH
      save
C     !DASH
      real*8 ASMCR, EPTAU, WNJNK, ZERO
      integer IOMX, IPZER, IQPGA, IQSRJ, IXASM, MS, MSEDG, MSEDW, NERM,
     $        NIASM, NL, NO, NS, jummy
      logical ION, MSOK, NSOK
      character PGA*3, SRJ*3
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- DIVIDER     as of 2003 Sep 29
      real*8      VSMLDV
      integer     MESDIV,MESDVZ,KNTDIV,KNTDVZ
      common      /DIVIDE1/ VSMLDV
      common      /DIVIDE2/ MESDIV,MESDVZ,KNTDIV,KNTDVZ
C     Parameters for subroutine "DIVIDE":
C       VSMLDV - replacement for B in A/B when B=0;
C       MESDIV - switch for A/0 error messages;
C       MESDVZ - switch for 0/0 error messages;
C       KNTDIV - number of times A/0 was detected;
C       KNTDVZ - number of times 0/0 was detected.
C     .
C     !DASH
      external  ONOFF, LINER, HI, BYE
C
      call HI ('KOHL')
C     !BEG
      call LINER (1, NO)
      write (NO,100) VSMLDV,IPZER
  100 format(' ','DIVIDE controls:  VSMLL = ',1PE12.4,5X,'IPZER = ',I1)
C
      call LINER (1, NO)
      if(WNJNK.gt.ZERO) then
        write (NO,101) WNJNK
  101   format(' ','Every WN-matrix-term whose magnitude is less than ',
     $             'WNJUNK =',1PE10.2,' will be set =0.')
      else
        write (NO,102)
  102   format(' ','No WN-matrix-term editing will be done.')
      end if
C     !EJECT
      if(ION) then
        call LINER   (1, NO)
        if(EPTAU.gt.ZERO) then
          write (NO,103) EPTAU
  103     format(' ','The TAU-criterion for automatic use of the ',
     $               'static escape probability method for Line Source ',
     $               'Function, ESCTAU =',1PE11.4)
        else
          write (NO,104) EPTAU
  104     format(' ','(Automtatic use of the static escape probability ',
     $               'method for Line Source Function is not allowed. '
     $               'ESCTAU =',1PE12.4,')')
        end if
        MSOK=(MS.ge.2).and.(MS.le.NL)
        NSOK=(NS.ge.1).and.(NS.lt.NL).and.(NS.lt.MS)
        if(MSOK.and.NSOK) then
          call LINER (1, NO)
          write (NO,105) MS,NS
  105     format(' ','The "Reference Transition" (MS/NS) ',
     $               'is (',I2,'/',I2,')')
        end if
        call LINER   (1, NO)
        write (NO,106) MSEDG,MSEDW
  106   format(' ','METSE defaults: METSEDG =',I2,' in general; ',
     $             'METSEDW =',I2,' for transitions down to ',
     $             'level 1.')
      end if
C
      call LINER     (1, NO)
      write (NO,107) NERM
  107 format(' ','Limit for "controlled" editing messages: NERM =',I5)
      if(NERM.gt.0) then
        write (NO,108)
  108   format(' ','(Note: to get all messages in very short form, ',
     $             'set NERM = -1.)')
      end if
C
      if(IOMX.gt.1) then
        call ONOFF   (IQPGA, jummy, PGA)
        call ONOFF   (IQSRJ, jummy, SRJ)
        write (NO,109) PGA,SRJ
  109   format(' ','(Note that option PEGTNALL = ',A,' and ',
     $             'option PESRJALL = ',A,'.)')
      end if
      call LINER     (1, NO)
      write (NO,110) ASMCR,NIASM,IXASM
  110 format(' ','Sequential SMOOTHing: ASMCR =',1PE9.1,', NIASM =',I4,
     $           ', IXASM =',I6,' (with IPEX = 15).'/
     $       ' ','                      For messages and dumps, use ',
     $           'options SQSMPRNT and SQSMDMP.')
C     !END
      call BYE ('KOHL')
C
      return
      end
