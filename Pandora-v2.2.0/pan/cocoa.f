      subroutine COCOA
     $(EDGT,NUMT,LEVT,KNT,IPNT,IWRK)
C
C     Rudolf Loeser, 1976 Feb 18
C---- Establishes a sorted table of absorption edges, EDGT, plus
C     associated ion numbers, NUMT, and level numbers, LEVT.
C     !DASH
      save
C     !DASH
      real*8 EDGT
      integer IPNT, IWRK, KNT, LEVT, NUMT
C     !DASH
      external CACAO, SORT, ORDERI, HI, BYE
C
C               EDGT(LEND), NUMT(LEND), LEVT(LEND), IPNT(LEND),
      dimension EDGT(*),    NUMT(*),    LEVT(*),    IPNT(*),
C
C               IWRK(LEND)
     $          IWRK(*)
C
      call HI ('COCOA')
C     !BEG
C---- Get raw data
      call CACAO  (EDGT, NUMT, LEVT, KNT)
C
C---- Sort edge table
      call SORT   (EDGT, KNT, IPNT, 'Continuum edges')
C
C---- Shuffle other tables into sort order
      call ORDERI (LEVT, IPNT, KNT, IWRK)
      call ORDERI (NUMT, IPNT, KNT, IWRK)
C     !END
      call BYE ('COCOA')
C
      return
      end
