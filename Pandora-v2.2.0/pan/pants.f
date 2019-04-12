      subroutine PANTS
     $(QNAME,WO)
C
C     Rudolf Loeser, 2002 Mar 12
C---- Reads obsolete RHO-weight-adjustment parameters.
C     (This is version 3 of PANTS.)
C     !DASH
      save
C     !DASH
      real*8 WO
      integer LUEO, jummy
      character QNAME*8, qummy*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MUSTARD, MESHED, MASHED, HI, BYE
C
      call HI ('PANTS')
C     !BEG
      call MUSTARD (QNAME, WO, jummy, qummy, 1, 5)
C
      call MESHED  ('PANTS', 3)
      write (LUEO,100) QNAME
  100 format(' ',A,' is an obsolete input quantity and should not ',
     $           'be used.')
      if(QNAME(:3).eq.'WMN') then
        write (LUEO,101)
  101   format(' ','The corresponding current quantity is WRMX; ',
     $             'it should = (1 - WMN).')
      else
        write (LUEO,102)
  102   format(' ','The corresponding current quantity is WRMN; ',
     $             'it should = (1 - WMX).')
      end if
      call MASHED  ('PANTS')
C     !END
      call BYE ('PANTS')
C
      return
      end
