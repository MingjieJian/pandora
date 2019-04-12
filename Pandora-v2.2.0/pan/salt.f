      subroutine SALT
     $(K,L,INCRAD,TCINT,TCINTA,TCFLX,BCIZ,BCZA,BCFZ)
C
C     Rudolf Loeser, 1992 May 11
C---- Sets up profile background data /Hz,
C     for a particular viewing position (front vs. back).
C
C     Note: the "B..." arrays were set up by PICULET using
C     K [= XLBL(MMK)]. (PICULET is called by VOLVOX for intensities,
C     and by NARKE for fluxes.)
C
C     (This is version 4 of SALT.)
C     !DASH
      save
C     !DASH
      real*8 BCFZ, BCIZ, BCZA, TCFLX, TCINT, TCINTA
      integer K, L
      logical INCRAD
C     !DASH
      external MOVE1, HI, BYE
C
C               TCINT(KM,L), TCINTA(KM), TCFLX(KM), BCZA(KM), BCFZ(KM),
      dimension TCINT(*),    TCINTA(*),  TCFLX(*),  BCZA(*),  BCFZ(*),
C
C               BCIZ(KM,L)
     $          BCIZ(*)
C
      call HI ('SALT')
C     !BEG
      call MOVE1   (BCIZ, (K*L), TCINT )
      call MOVE1   (BCFZ, K    , TCFLX )
      if(INCRAD) then
        call MOVE1 (BCZA, K    ,TCINTA)
      end if
C     !END
      call BYE ('SALT')
C
      return
      end
