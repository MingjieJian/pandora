      subroutine MESHOW
     $(NO)
C     Rudolf Loeser, 1987 Dec 02
C---- Prints MEMOIR buffer control parameters and activity counts.
C     !DASH
      save
C     !DASH
      real*8 EIGHT, XBA, XBR, XBU
      integer NO
C     !COM
C---- CORTEX      as of 1997 Jun 16
      integer     MELEFT,MENEXT,MENRAC,MENRUP,MENRRE,MELIMT
      real*8      SIOBUF,SENWAC,SENWUP,SENWRE
C
      dimension   SIOBUF( 1 )
C     (The   r e a l   length of SIOBUF is set in PANDORA!)
C
      common      /CORTEX1/ MELEFT,MENEXT,MENRAC,MENRUP,MENRRE
      common      /CORTEX2/ SENWAC,SENWUP,SENWRE
      common      /CORTEX3/ MELIMT
      common      /CORTEX4/ SIOBUF
C
C     Control parameters for the MEMOIR subroutines:
C
C     MELIMT = length of entire buffer (SIOBUF), in words;
C     MELEFT = number of unused words left in buffer;
C     MENEXT = index of next available word in buffer;
C     MENRAC = number of logical records accepted;
C     MENRUP = number of logical records updated;
C     MENRRE = number of logical records returned;
C     SENWAC = number of words accepted;
C     SENWUP = number of words updated;
C     SENWRE = number of words returned.
C     .
C     !DASH
      data EIGHT /8.D0/
C
C     !BEG
      if(NO.gt.0) then
        write (NO,100) MELIMT,MENEXT,MELEFT
  100   format(' ','MEMOIR: "in-memory" scratch file.'//
     $         ' ','Buffer length =',I10,' (set at compile-time).'/
     $         ' ','Next available word =',I10,10X,
     $             'Number of unused words remaining =',I10/)
  101   format(' ',I10,' records were ',A8,
     $             '; altogether they comprised ',1PE11.4,
     $             ' words ( = ',E11.4,' bytes).')
        XBA = SENWAC*EIGHT
        write (NO,101) MENRAC,'accepted',SENWAC,XBA
        XBR = SENWRE*EIGHT
        write (NO,101) MENRRE,'returned',SENWRE,XBR
        XBU = SENWUP*EIGHT
        write (NO,101) MENRUP,' updated',SENWUP,XBU
      end if
C     !END
C
      return
      end
