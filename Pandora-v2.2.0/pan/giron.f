      subroutine GIRON
     $(KXMAX)
C
C     Rudolf Loeser, 1992 Feb 26
C---- Updates KM (if needed), and prints lengths.
C     (This is version 2 of GIRON.)
C     !DASH
      save
C     !DASH
      integer KM, KMSAV, KXMAX, LDLMU, LDLMX, LDLSV, NO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(49),KM )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(53),LDLMU)
      equivalence (LEST(33),LDLMX)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external LINER, HALT, HI, BYE
C
      call HI ('GIRON')
C     !BEG
      KMSAV = KM
      if((KXMAX.gt.0).and.(KM.ne.KXMAX)) then
C----   KM should be equal to the length of the longest XIFUL table.
C       Its value was originally estimated by THESEUS.
C       Now that all "special XI-tables" have been set up, the length
C       of the actual longest XIFUL table is known, and a more
C       accurate vale of KM can now be assigned:
        KM = KXMAX
C       (This is important because KM is used to allocate storage.)
      end if
C
      LDLSV = LDLMX
      if((LDLMU.gt.1).and.(LDLMX.ne.LDLMU)) then
C----   LDLMX should be equal to the length of the longest DDL table.
C       Its input value is an imposed upper limit; now that the
C       length of the actual longest DDL table is known, a more
C       accurate value of LDLMX can be assigned:
        LDLMX = LDLMU
C       (This is important because LDLMX is used to allocate storage.)
      end if
C
      call LINER  (2,NO)
      write (NO,100) KMSAV,KM,LDLSV,LDLMX
  100 format(' ',19X,'(Estimated KM =',I6,3X,'actual KM =',I6,9X,
     $           'Input LDLMX =',I4,3X,'actual LDLMX =',I4,')')
C
      if(KMSAV.lt.KM) then
        write (MSSLIN(1),101)
  101   format('Actual KM   m a y  n o t   be less than Estimated KM. ',
     $         '(See input parameter KMMAX.)')
        call HALT ('GIRON', 1)
      end if
C     !END
      call BYE ('GIRON')
C
      return
      end
