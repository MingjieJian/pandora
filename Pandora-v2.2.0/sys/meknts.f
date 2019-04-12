      subroutine MEKNTS
     $(NRA,NRU,NRR,XNWA,XNWU,XNWR)
C     Rudolf Loeser, 1987 Dec 02
C---- Returns "in-memory" scratch file activity counts
C     (see remarks in MEMOIR).
C     !DASH
      save
C     !DASH
      real*8 XNWA, XNWR, XNWU
      integer NRA, NRR, NRU
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
C
C     !BEG
      NRA  = MENRAC
      NRU  = MENRUP
      NRR  = MENRRE
      XNWA = SENWAC
      XNWU = SENWUP
      XNWR = SENWRE
C     !END
C
      return
      end
