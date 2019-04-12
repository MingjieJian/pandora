      subroutine AMBROSE
     $(CORE,DL,K,KODE,IU,IL,NO,EDGT,NUMT,LEVT,IPNT,IWRK)
C
C     Rudolf Loeser, 1976 Feb 18
C---- Determines whether the frequency range for background continuum
C     data for Line (IU,IL) contains an absorption edge, and,
C     if yes, prints a message to that effect.
C     !DASH
      save
C     !DASH
      real*8 CORE, DL, EDGT, WHI, WLO
      integer IL, IPNT, IU, IWRK, K, KHI, KLO, KNT, KODE, LEVT, NO,
     $        NUMT
      logical EDGES
C     !DASH
      external COCOA, MEAD, DREGS, HI, BYE
C
C               DL(K), EDGT(LEND), NUMT(LEND), LEVT(LEND), IPNT(LEND),
      dimension DL(*), EDGT(*),    NUMT(*),    LEVT(*),    IPNT(*),
C
C               IWRK(LEND)
     $          IWRK(*)
C
      call HI ('AMBROSE')
C     !BEG
      if(NO.gt.0) then
        call COCOA   (EDGT, NUMT, LEVT, KNT, IPNT, IWRK)
C
        if(KODE.eq.1) then
          WLO = CORE-DL(K)
        else
          WLO = CORE+DL(1)
        end if
        WHI = CORE+DL(K)
        call MEAD    (EDGT, KNT, WLO, KLO, WHI, KHI, EDGES)
C
        if(EDGES) then
          call DREGS (EDGT, NUMT, LEVT, KNT, NO, IU, IL, WLO, KLO,
     $                WHI, KHI, CORE, DL, K, KODE)
        end if
      end if
C     !END
      call BYE ('AMBROSE')
C
      return
      end
