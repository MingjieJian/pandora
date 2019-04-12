      subroutine PICULET
     $(I,NW,J,K,L,LFB,EMU,PHZ,PA,BT,BPHZ,BPA,BBT,PHZA,PAA,BTA,
     $ BPHZA,BPAA,BBTA)
C
C     Rudolf Loeser, 1986 Mar 07
C---- Transfers continuum data into slots in a
C     Line Intensity data block (i.e. into arrays named "B...").
C
C     Note: J.eq.0 for frequency-independent background, so that all K
C     values must be set to the constant value pertaining to that MU;
C     1. le. J .le. K for frequency-dependent background, so only the
C     values for all MU at this J are to be set up.
C     !DASH
      save
C     !DASH
      real*8 BBT, BBTA, BPA, BPAA, BPHZ, BPHZA, BT, BTA, EMU, PA, PAA,
     $       PHZ, PHZA
      integer I, J, K, L, LFB, M, NW
      logical INCRAD
C     !DASH
      external MOVED, SET1, DOWNY, HI, BYE
C
C               PAA(NW), PA(NW,L), BT(NW,L), PHZA(NW), BBTA(K), EMU(L),
      dimension PAA(*),  PA(NW,*), BT(NW,*), PHZA(*),  BBTA(*), EMU(*),
C
C               PHZ(NW,L), BPHZ(K,L), BPA(K,L), BPHZA(K), BBT(K,L),
     $          PHZ(NW,*), BPHZ(K,*), BPA(K,*), BPHZA(*), BBT(K,*),
C
C               BPAA(K), BTA(NW)
     $          BPAA(*), BTA(*)
C     !EJECT
C
      call HI ('PICULET')
C     !BEG
      call DOWNY    (LFB,EMU(1),INCRAD)
C
      if((J.ge.1).and.(J.le.K)) then
C
        call MOVED  (PHZ(I,1),NW,L,BPHZ(J,1),K,L)
        call MOVED  (PA(I,1) ,NW,L,BPA(J,1) ,K,L)
        call MOVED  (BT(I,1) ,NW,L,BBT(J,1) ,K,L)
C
        if(INCRAD) then
          BPHZA(J) = PHZA(I)
          BPAA(J)  = PAA(I)
          BBTA(J)  = BTA(I)
        end if
C
      else
C
        do 100 M = 1,L
          call SET1 (BPHZ(1,M),K,PHZ(I,M))
          call SET1 (BPA(1,M) ,K,PA(I,M) )
          call SET1 (BBT(1,M) ,K,BT(I,M) )
  100   continue
C
        if(INCRAD) then
          call SET1 (BPHZA    ,K,PHZA(I) )
          call SET1 (BPAA     ,K,PAA(I)  )
          call SET1 (BBTA     ,K,BTA(I)  )
        end if
C
      end if
C     !END
      call BYE ('PICULET')
C
      return
      end
